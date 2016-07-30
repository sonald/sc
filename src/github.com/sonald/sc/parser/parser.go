package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"io"
	"math/rand"
	"reflect"
	"runtime"
	"strings"
)

//maximum lookaheads
const NR_LA = 4

type Parser struct {
	lex             *lexer.Scanner
	tokens          [NR_LA]lexer.Token // support 4-lookahead
	cursor          int
	eot             bool // meet EOT
	ctx             *AstContext
	currentScope    *SymbolScope
	tu              *TranslationUnit
	effectiveParent Ast // This is a bad name, it is used for RecordDecl parsing
	verbose         bool
}

type ParseOption struct {
	filename    string
	reader      io.Reader
	dumpAst     bool
	dumpSymbols bool
	verbose     bool // log call trace
}

func NewParser() *Parser {
	p := &Parser{}

	var top = SymbolScope{}
	p.ctx = &AstContext{top: &top}
	p.currentScope = &top

	return p
}

func (self *Parser) peek(n int) lexer.Token {
	if n >= NR_LA || n < 0 {
		panic(fmt.Sprintf("only maximum of %d tokens lookahead supported", NR_LA))
	}

	tok := self.tokens[n]
	util.Printf("peek %s(%s)\n", lexer.TokKinds[tok.Kind], tok.AsString())
	return tok
}

func (self *Parser) getNextToken() lexer.Token {
	if self.eot {
		return lexer.Token{Kind: lexer.EOT}
	}

	tok := self.lex.Next()
	if tok.Kind == lexer.EOT {
		self.eot = true
	}
	return tok
}

func (self *Parser) next() lexer.Token {
	tok := self.tokens[0]
	for i := 1; i <= NR_LA-1; i++ {
		self.tokens[i-1] = self.tokens[i]
	}
	self.tokens[NR_LA-1] = self.getNextToken()
	//util.Printf("next %s(%s)\n", lexer.TokKinds[tok.Kind], tok.AsString())
	return tok
}

func (self *Parser) match(kd lexer.Kind) {
	if self.peek(0).Kind == kd {
		self.next()
	} else {
		self.parseError(self.peek(0),
			fmt.Sprintf("expect %s, but %s found",
				lexer.TokKinds[kd], lexer.TokKinds[self.peek(0).Kind]))
	}
}

// the only entry
func (self *Parser) Parse(opts *ParseOption) Ast {
	self.lex = lexer.NewScanner(opts.reader)
	for i := range self.tokens {
		self.tokens[i] = self.getNextToken()
	}

	self.verbose = opts.verbose

	return self.parseTU(opts)
}

// translation-unit: external-declaration+
func (self *Parser) parseTU(opts *ParseOption) Ast {
	defer self.trace("")()

	self.tu = &TranslationUnit{}
	self.tu.filename = opts.filename
	self.effectiveParent = self.tu
	self.ctx.top.Owner = self.tu

	for self.peek(0).Kind != lexer.EOT {
		self.parseExternalDecl()
	}
	return self.tu
}

var storages map[string]Storage

var typeSpecifier map[string]bool

var typeQualifier map[string]Qualifier

func isStorageClass(tok lexer.Token) bool {
	_, ok := storages[tok.AsString()]
	return ok
}

func isTypeSpecifier(tok lexer.Token) bool {
	_, ok := typeSpecifier[tok.AsString()]
	return ok
}

func isTypeQualifier(tok lexer.Token) bool {
	_, ok := typeQualifier[tok.AsString()]
	return ok
}

func (self *Parser) parseError(tok lexer.Token, msg string) {
	panic(fmt.Sprintf("tok %s(%s) %d:%d, %s", lexer.TokKinds[tok.Kind], tok.AsString(),
		tok.Line, tok.Column, msg))
}

func (self *Parser) parseTypeDecl(sym *Symbol) {
	defer self.trace("")()

	var ty SymbolType

	for {
		tok := self.peek(0)
		if tok.Kind == lexer.KEYWORD {
			if isStorageClass(tok) {
				self.next()
				if sym.Storage == NilStorage {
					sym.Storage = storages[tok.AsString()]
				} else {
					self.parseError(tok, "multiple storage class specified")
				}
			} else if isTypeSpecifier(tok) {
				if sym.Type != nil {
					if _, qualified := sym.Type.(*QualifiedType); !qualified {
						self.parseError(tok, "multiple type specifier")
					}
				}
				switch tok.AsString() {
				case "int":
					self.next()
					ty = &IntegerType{}
				case "float":
					self.next()
					ty = &FloatType{}
				case "union", "struct":
					ty = self.parseRecordType()

				default:
					self.parseError(tok, "not implemented")
				}

				if sym.Type == nil {
					sym.Type = ty
				} else {
					var qty = sym.Type.(*QualifiedType)
					for qty.Base != nil {
						qty = qty.Base.(*QualifiedType)
					}
					qty.Base = ty
				}

			} else if isTypeQualifier(tok) {
				self.next()
				sym.Type = &QualifiedType{Base: sym.Type, Qualifier: typeQualifier[tok.AsString()]}
				//self.parseError(tok, "multiple type qualifier specified")
			} else if tok.AsString() == "inline" {
				self.next()
				//ignore now
			} else {
				self.parseError(tok, "invalid declaration specifier")
			}
			//FIXME: handle usertype by typedef
		} else {
			break
		}
	}

	util.Printf("parsed type template %v", sym)
}

func (self *Parser) parseFunctionParams(decl *FunctionDecl) {
	defer self.trace("")()

	funcSym := self.LookupSymbol(decl.Name)
	var ty = funcSym.Type.(*Function)
	for {
		if self.peek(0).Kind == lexer.RPAREN {
			break
		}

		var tmpl = &Symbol{}
		self.parseTypeDecl(tmpl)
		if arg := self.parseDeclarator(tmpl); arg == nil {
			break
		} else {
			switch arg.(type) {
			case *VariableDecl:
				var pd = &ParamDecl{decl.Node, arg.(*VariableDecl).Sym}
				decl.Args = append(decl.Args, pd)

				pty := decl.Scope.LookupSymbol(pd.Sym, false)
				ty.Args = append(ty.Args, pty.Type)
				util.Printf("parsed arg %v", pd.Repr())
			default:
				self.parseError(self.peek(0), "invalid parameter declaration")
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}
}

//FIXME: support full c99 declarator parsing
//FIXME: check redeclaration
func (self *Parser) parseDeclarator(sym *Symbol) Ast {
	defer self.trace("")()

	var newSym = Symbol{Type: sym.Type, Storage: sym.Storage}
	self.AddSymbol(&newSym)

	var decl Ast

	tok := self.next()
	if tok.Kind == lexer.MUL {
		newSym.Type = &Pointer{newSym.Type}
		tok = self.next()
	}

	if tok.Kind != lexer.IDENTIFIER {
		self.parseError(tok, "expect identifier")
	}
	newSym.Name = tok

	switch self.peek(0).Kind {
	case lexer.OPEN_BRACKET: // array
		self.match(lexer.OPEN_BRACKET)
		if tok := self.peek(0); tok.Kind == lexer.INT_LITERAL {
			newSym.Type = &Array{newSym.Type, 1, []int{tok.AsInt()}}
			self.next()
		} else if tok.Kind == lexer.CLOSE_BRACKET {
			newSym.Type = &Array{newSym.Type, 1, []int{-1}} // NOTE: I use -1 means don't know
		} else {
			self.parseError(tok, "invalid array type specifier")
		}
		self.match(lexer.CLOSE_BRACKET)

		decl = &VariableDecl{Sym: newSym.Name.AsString()}

	case lexer.LPAREN: // func
		self.match(lexer.LPAREN)
		var fdecl = &FunctionDecl{}
		decl = fdecl
		newSym.Type = &Function{Return: newSym.Type}
		fdecl.Name = newSym.Name.AsString()
		// when found definition of func, we need to chain fdecl.Scope with body
		fdecl.Scope = self.PushScope()
		fdecl.Scope.Owner = fdecl
		self.parseFunctionParams(fdecl)
		self.PopScope()
		self.match(lexer.RPAREN)

	default:
		decl = &VariableDecl{Sym: newSym.Name.AsString()}
	}

	if self.peek(0).Kind == lexer.ASSIGN {
		// parse initializer
		self.next()
		switch decl.(type) {
		case *VariableDecl:
			decl.(*VariableDecl).init = self.parseInitializerList()
		default:
			self.parseError(self.peek(0), "Initializer is not allowed here")
		}
	}
	return decl
}

/**
struct-or-union-specifier:
	struct-or-union identifier? { struct-declaration-list }
	struct-or-union identifier

struct-or-union: struct | union

struct-declaration-list: struct-declaration+

struct-declaration: specifier-qualifier-list struct-declarator-list ;

specifier-qualifier-list: ( type-specifier | type-qualifier ) *

struct-declarator-list: struct-declarator
	struct-declarator-list , struct-declarator

struct-declarator:
	declarator
	declarator? : constant-expression
*/

func (self *Parser) parseRecordType() SymbolType {
	defer self.trace("")()
	var (
		recDecl = &RecordDecl{Node: Node{self.ctx}}
		recSym  = &Symbol{}
		ret     = &RecordType{}
		tok     lexer.Token
	)

	tok = self.next()
	ret.Union = tok.AsString() == "union"

	if tok = self.next(); tok.Kind == lexer.IDENTIFIER {
		ret.Name = tok.AsString()
		recSym.Name = tok
		if ty := self.LookupUserType(ret.Name); ty != nil {
			return ty
		}
	} else {
		ret.Name = NextAnonyRecordName()
	}
	recSym.Type = ret
	recDecl.Sym = ret.Name

	self.match(lexer.LBRACE)

	//NOTE: we register here so that pointer of this type can be used as field type
	//FIXME: if parse failed, need to deregister it
	self.AddUserType(ret)
	self.AddTypeSymbol(recSym)
	recDecl.Scope = self.PushScope()
	recDecl.Scope.Owner = recDecl

	defer func() {
		self.PopScope()
		if p := recover(); p == nil {
			if ds, ok := self.effectiveParent.(*DeclStmt); ok {
				ds.RecordDecls = append(ds.RecordDecls, recDecl)
			} else {
				self.tu.recordDecls = append(self.tu.recordDecls, recDecl)
			}
		} else {
			// if this is top level of record decl, skip it and continue
			if _, ok := self.currentScope.Owner.(*RecordDecl); !ok {
				util.Printf(util.Parser, util.Warning, p)
				for tok := self.next(); tok.Kind != lexer.RBRACE; tok = self.next() {
				}
				self.mayIgnore(lexer.SEMICOLON)
			}
			panic(p) //propagate
		}
	}()

	for {
		if self.peek(0).Kind == lexer.RBRACE {
			break
		}

		var tmplTy SymbolType
		var tmplSym = &Symbol{}
		var loc = self.peek(0).Location

		for {
			tok := self.peek(0)
			if tok.Kind == lexer.KEYWORD {
				if isTypeSpecifier(tok) {
					if tmplSym.Type != nil {
						if _, qualified := tmplSym.Type.(*QualifiedType); !qualified {
							self.parseError(tok, "multiple type specifier")
						}
					}
					switch tok.AsString() {
					case "int":
						self.next()
						tmplTy = &IntegerType{}
					case "float":
						self.next()
						tmplTy = &FloatType{}
					case "union", "struct":
						tmplTy = self.parseRecordType()

					default:
						self.parseError(tok, "not implemented")
					}

					if tmplSym.Type == nil {
						tmplSym.Type = tmplTy
					} else {
						var qty = tmplSym.Type.(*QualifiedType)
						for qty.Base != nil {
							qty = qty.Base.(*QualifiedType)
						}
						qty.Base = tmplTy
					}

				} else if isTypeQualifier(tok) {
					self.next()
					tmplSym.Type = &QualifiedType{Base: tmplSym.Type, Qualifier: typeQualifier[tok.AsString()]}
				} else {
					self.parseError(tok, "invalid field type specifier")
				}
			} else {
				break
			}
		}

		util.Printf("parsed field type template %v", tmplSym)

		for {
			if self.peek(0).Kind == lexer.SEMICOLON {
				self.next()
				break
			}

			var fd = &FieldDecl{Node: Node{self.ctx}}
			var ft = &FieldType{}

			if self.peek(0).Kind != lexer.COLON {
				// FIXME: parseDeclarator will add new symbol into current scope,
				// which will pollute scoping rule
				var decl = self.parseDeclarator(tmplSym)
				switch decl.(type) {
				case *VariableDecl:
					var vd = decl.(*VariableDecl)
					var vs = self.LookupSymbol(vd.Sym)

					fd.Loc = vs.Name.Location
					fd.Sym = vd.Sym
					recDecl.Fields = append(recDecl.Fields, fd)

					ft.Base = vs.Type
					ft.Name = fd.Sym

				default:
					self.parseError(self.peek(0), "invalid field declarator")
				}
			} else {
				fd.Loc = loc
				fd.Sym = NextAnonyFieldName(recDecl.Sym)
				recDecl.Fields = append(recDecl.Fields, fd)

				ft.Base = tmplSym.Type
				ft.Name = fd.Sym
			}

			// FIXME: parse an const expr here, but in that case, a SymbolType
			// may contain an Expression (Ast) node which feels weird to me.
			if self.peek(0).Kind == lexer.COLON {
				self.next()
				tag := self.peek(0).AsInt()
				ft.Tag = &tag
				self.match(lexer.INT_LITERAL)
			}

			util.Printf("parsed field type %v", ft)
			ret.Fields = append(ret.Fields, ft)
			if self.peek(0).Kind == lexer.COMMA {
				self.next()
			}
		}
	}

	self.match(lexer.RBRACE)

	util.Printf("parsed RecordType: %v", ret)
	return ret
}

func (self *Parser) parseExternalDecl() Ast {
	defer self.trace("")()
	defer self.handlePanic(lexer.SEMICOLON)

	var tmpl = &Symbol{}
	self.parseTypeDecl(tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(tmpl); decl == nil {
			break
		} else {
			switch decl.(type) {
			case *VariableDecl:
				self.tu.varDecls = append(self.tu.varDecls, decl.(*VariableDecl))
				util.Printf("parsed %v", decl.Repr())
			case *FunctionDecl:
				var fdecl = decl.(*FunctionDecl)
				self.tu.funcDecls = append(self.tu.funcDecls, fdecl)
				util.Printf("parsed %v", decl.Repr())

				if self.peek(0).Kind == lexer.LBRACE {
					if self.currentScope != fdecl.Scope.Parent {
						panic("fdecl should inside currentScope")
					}
					self.currentScope = fdecl.Scope
					fdecl.Body = self.parseCompoundStmt()
					self.PopScope()

					// parse of function definition done
					goto done
				}

			default:
				self.parseError(self.peek(0), "")
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}

done:
	return nil
}

func (self *Parser) parseCompoundStmt() *CompoundStmt {
	defer self.trace("")()
	defer self.handlePanic(lexer.RBRACE)

	var scope = self.PushScope()
	defer func() { self.PopScope() }()
	var compound = &CompoundStmt{Node: Node{self.ctx}, Scope: scope}
	scope.Owner = compound

	self.match(lexer.LBRACE)

	for {
		if self.peek(0).Kind == lexer.RBRACE {
			break
		}

		if stmt := self.parseStatement(); stmt != nil {
			compound.Stmts = append(compound.Stmts, stmt)
		}
	}
	self.match(lexer.RBRACE)
	return compound
}

func (self *Parser) parseStatement() Statement {
	defer self.trace("")()
	defer self.handlePanic(lexer.SEMICOLON)

	tok := self.peek(0)

	var stmt Statement
	// all normal statements

	// else
	switch tok.Kind {
	case lexer.KEYWORD:
		switch tok.AsString() {
		case "if":
			stmt = self.parseIfStatement()

		case "switch":
			stmt = self.parseSwitchStatement()
		case "case":
			stmt = self.parseCaseStatement()
		case "default":
			stmt = self.parseDefaultStatement()
		case "while":
			stmt = self.parseWhileStatement()
		case "do":
			stmt = self.parseDoStatement()
		case "for":
			stmt = self.parseForStatement()
		case "goto":
			stmt = self.parseGotoStatement()
		case "continue":
			stmt = self.parseContinueStatement()
		case "break":
			stmt = self.parseBreakStatement()
		case "return":
			stmt = self.parseReturnStatement()
		case "sizeof":
			// sizeof expr
			stmt = self.parseExprStatement()

		default:
			if isStorageClass(tok) || isTypeQualifier(tok) || isTypeSpecifier(tok) {
				stmt = self.parseDeclStatement()
			} else {

				self.parseError(tok, "unknown keyword")
			}
		}

	default:
		if tok.Kind == lexer.LBRACE {
			stmt = self.parseCompoundStmt()
		} else if tok.Kind == lexer.IDENTIFIER && self.peek(1).Kind == lexer.COLON {
			stmt = self.parseLabelStatement()
		} else {
			stmt = self.parseExprStatement()
		}
	}

	if reflect.ValueOf(stmt).IsNil() {
		stmt = nil
	}

	if stmt != nil {
		util.Printf("parsed stmt %s\n", reflect.TypeOf(stmt).Elem().Name())
	}
	return stmt
}

func (self *Parser) parseIfStatement() *IfStmt {
	defer self.trace("")()

	var ifStmt = &IfStmt{Node: Node{self.ctx}}
	self.next() // eat if
	self.match(lexer.LPAREN)
	ifStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	ifStmt.TrueBranch = self.parseStatement()
	if self.peek(0).AsString() == "else" {
		self.next()
		ifStmt.FalseBranch = self.parseStatement()
	}

	return ifStmt
}
func (self *Parser) parseSwitchStatement() *SwitchStmt {
	defer self.trace("")()

	var switchStmt = &SwitchStmt{Node: Node{self.ctx}}
	self.next()
	self.match(lexer.LPAREN)
	switchStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	switchStmt.Body = self.parseStatement()

	return switchStmt
}

func (self *Parser) tolerableParse(rule func() Ast, follow ...lexer.Token) (retVal Ast) {
	defer self.trace("")()
	defer func() {
		if p := recover(); p != nil {
			util.Printf(util.Parser, util.Critical, "Parse Error, ignore until %v\n", follow[0])
			tok := self.peek(0)
			for {
				if tok.Kind == lexer.EOT {
					panic(p)
				}
				for _, next := range follow {
					if next.Kind == tok.Kind {
						return
					}
				}
				tok = self.next()
			}
			retVal = nil
		}
	}()

	retVal = rule()
	return
}

func (self *Parser) mayIgnore(exp lexer.Kind) bool {
	if self.peek(0).Kind == exp {
		self.next()
		return true
	} else {
		return false
	}
}

func (self *Parser) parseDoStatement() *DoStmt {
	defer self.trace("")()

	var (
		doStmt = &DoStmt{Node: Node{self.ctx}}
		tok    lexer.Token
	)

	self.next()
	doStmt.Body = self.parseStatement()
	if tok = self.next(); tok.AsString() != "while" {
		self.parseError(tok, "exepect while")
	}
	self.match(lexer.LPAREN)
	doStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	self.mayIgnore(lexer.SEMICOLON)

	return doStmt
}

func (self *Parser) parseWhileStatement() *WhileStmt {
	defer self.trace("")()

	var (
		whileStmt = &WhileStmt{Node: Node{self.ctx}}
	)

	self.next()
	self.match(lexer.LPAREN)
	whileStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	whileStmt.Body = self.parseStatement()

	return whileStmt
}

func (self *Parser) parseLabelStatement() *LabelStmt {
	defer self.trace("")()

	var labelStmt = &LabelStmt{Node: Node{self.ctx}}

	tok := self.next()
	if tok.Kind != lexer.IDENTIFIER {
		self.parseError(tok, "expect identifier")
	}
	labelStmt.Label = tok.AsString()
	self.match(lexer.COLON)
	labelStmt.Stmt = self.parseStatement()
	return labelStmt
}

func (self *Parser) parseGotoStatement() *GotoStmt {
	defer self.trace("")()

	var gotoStmt = &GotoStmt{Node: Node{self.ctx}}

	self.next()
	tok := self.next()
	if tok.Kind != lexer.IDENTIFIER {
		self.parseError(tok, "expect identifier")
	}
	gotoStmt.Label = tok.AsString()
	self.mayIgnore(lexer.SEMICOLON)
	return gotoStmt
}

func (self *Parser) parseContinueStatement() *ContinueStmt {
	defer self.trace("")()

	var continueStmt = &ContinueStmt{Node: Node{self.ctx}}

	self.next()
	self.mayIgnore(lexer.SEMICOLON)

	return continueStmt
}

func (self *Parser) parseBreakStatement() *BreakStmt {
	defer self.trace("")()

	var breakStmt = &BreakStmt{Node: Node{self.ctx}}

	self.next()
	self.mayIgnore(lexer.SEMICOLON)

	return breakStmt
}

func (self *Parser) parseReturnStatement() *ReturnStmt {
	defer self.trace("")()

	var returnStmt = &ReturnStmt{Node: Node{self.ctx}}

	self.next()
	if self.peek(0).Kind != lexer.SEMICOLON {
		returnStmt.Expr = self.parseExpression(0)
	}
	self.mayIgnore(lexer.SEMICOLON)

	return returnStmt
}

// there are two kinds of for ...
func (self *Parser) parseForStatement() *ForStmt {
	defer self.trace("")()

	var (
		forStmt  = &ForStmt{Node: Node{self.ctx}}
		tok      lexer.Token
		newScope bool = false
	)
	self.next()
	self.match(lexer.LPAREN)

	defer func() {
		if newScope {
			self.PopScope()
		}
	}()

	forStmt.Scope = self.currentScope
	//FIXME: only auto/static is allowed storage class here
	//FIXME: so struct decl itself is not auto or static
	tok = self.peek(0)
	if isStorageClass(tok) || isTypeQualifier(tok) || isTypeSpecifier(tok) {
		util.Println("parse decl in for")
		forStmt.Scope = self.PushScope()
		newScope = true
		forStmt.Decl = self.parseDeclStatement()
	} else {
		forStmt.Init = self.parseExpression(0)
		self.match(lexer.SEMICOLON)
	}

	forStmt.Cond = self.parseExpression(0)
	self.match(lexer.SEMICOLON)

	forStmt.Step = self.parseExpression(0)

	self.match(lexer.RPAREN)
	forStmt.Body = self.parseStatement()

	return forStmt
}

func (self *Parser) parseCaseStatement() *CaseStmt {
	defer self.trace("")()

	var caseStmt = &CaseStmt{Node: Node{self.ctx}}
	self.next()
	caseStmt.ConstExpr = self.parseExpression(0)
	self.match(lexer.COLON)
	caseStmt.Stmt = self.parseStatement()

	return caseStmt
}

func (self *Parser) parseDefaultStatement() *DefaultStmt {
	defer self.trace("")()

	var defaultStmt = &DefaultStmt{Node: Node{self.ctx}}
	self.next()
	self.match(lexer.COLON)
	defaultStmt.Stmt = self.parseStatement()

	return defaultStmt
}

func (self *Parser) parseDeclStatement() *DeclStmt {
	defer self.trace("")()

	var declStmt = &DeclStmt{Node: Node{self.ctx}}
	var prevParent = self.effectiveParent
	self.effectiveParent = declStmt

	defer func() {
		self.effectiveParent = prevParent
	}()

	var tmpl = &Symbol{}
	self.parseTypeDecl(tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(tmpl); decl == nil {
			break
		} else {
			switch decl.(type) {
			case *VariableDecl:
				declStmt.Decls = append(declStmt.Decls, decl.(*VariableDecl))
				util.Printf("parsed %v", decl.Repr())

			default:
				self.parseError(self.peek(0), "invalid declaration inside block")
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}

	return declStmt
}

func (self *Parser) parseExprStatement() (ret *ExprStmt) {
	defer self.trace("")()

	var exprStmt = &ExprStmt{Node: Node{self.ctx}}

	exprStmt.Expr = self.tolerableParse(func() Ast {
		return self.parseExpression(0)
	}, lexer.MakeToken(lexer.SEMICOLON, ";"))
	self.mayIgnore(lexer.SEMICOLON)

	if exprStmt.Expr == nil {
		util.Printf(util.Parser, util.Warning, "null expression")
		return nil
	}

	return exprStmt
}

type Associativity int
type Pred int
type Arity int

const (
	NoAssoc Associativity = 0 << iota
	RightAssoc
	LeftAssoc
)

const (
	NoneArity = 0
	Unary     = 1
	Binary    = 2
	Ternary   = 3
)

// one token may be used ad prefix or postfix/infix, so we need two Precedences for a token
type operation struct {
	lexer.Token
	Associativity
	NudPred int
	LedPred int
	nud     func(p *Parser, op *operation) Expression
	led     func(p *Parser, lhs Expression, op *operation) Expression
}

// operation templates
var operations map[lexer.Kind]*operation

// alloc new operation by copying specific template, this is only useful
// when tok value is needed such as IDENTIFIER
func (self *Parser) newOperation(tok lexer.Token) *operation {
	var op operation
	if _, valid := operations[tok.Kind]; valid {
		op = *operations[tok.Kind]
		op.Token = tok
	} else {
		op = *operations[lexer.ERROR]
	}
	return &op
}

// for binary op
// handle comma carefully
func binop_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()

	p.next() // eat op
	rhs := p.parseExpression(op.LedPred)

	var expr = &BinaryOperation{Node{p.ctx}, op.Token.Kind, lhs, rhs}
	util.Printf("parsed %v", expr.Repr())
	return expr
}

func assign_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()

	p.next() // eat op
	rhs := p.parseExpression(op.LedPred)

	var expr = &CompoundAssignExpr{Node{p.ctx}, op.Token.Kind, lhs, rhs}
	util.Printf("parsed %v", expr.Repr())
	return expr
}

// ?:
func condop_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	var expr = &ConditionalOperation{Node: Node{p.ctx}}

	expr.Cond = lhs

	p.next() // eat ?
	expr.True = p.parseExpression(op.LedPred)
	p.match(lexer.COLON) // eat :

	expr.False = p.parseExpression(op.LedPred)

	util.Printf("parsed %v", expr.Repr())
	return expr
}

// for unary (including prefix)
func unaryop_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	p.next()
	var expr = p.parseExpression(op.NudPred)
	return &UnaryOperation{Node{p.ctx}, op.Kind, false, expr}
}

// for postfix
func unaryop_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	p.next()

	return &UnaryOperation{Node{p.ctx}, op.Kind, true, lhs}
}

// e1.e2  e1->e2
func member_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	p.next()

	var expr = &MemberExpr{Node: Node{p.ctx}}
	expr.Target = lhs
	expr.Member = p.parseExpression(op.LedPred)
	return expr
}

// e1[e2]
func array_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	p.match(lexer.OPEN_BRACKET)

	var expr = &ArraySubscriptExpr{Node: Node{p.ctx}}
	expr.Target = lhs
	expr.Sub = p.parseExpression(op.LedPred)
	p.match(lexer.CLOSE_BRACKET)
	return expr
}

// could be funcall
func lparen_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	p.match(lexer.LPAREN)
	var expr = &FunctionCall{Node: Node{p.ctx}}
	expr.Func = lhs
	oldpred := operations[lexer.COMMA].LedPred

	//NOTE: there is a trick here:
	// there is a conflict when parsing args of form `expr, expr ...`,
	// which will be parsed as `comma expr`, so to handle this correctly,
	// I temperarily mark COMMA as END-OF-EXPR, and restore precedence later
	operations[lexer.COMMA].LedPred = -1

	for {
		if p.peek(0).Kind == lexer.RPAREN {
			break
		}

		expr.Args = append(expr.Args, p.parseExpression(0))
		if p.peek(0).Kind == lexer.COMMA {
			p.next()
		}
	}

	operations[lexer.COMMA].LedPred = oldpred
	p.match(lexer.RPAREN)
	return expr
}

/*
NOTE: this is messy, right now, only simple types supported
type-name:

specifier-qualifier-list abstract-declarator?

abstract-declarator: pointer

	pointer? direct-abstract-declarator

direct-abstract-declarator: ( abstract-declarator )

direct-abstract-declarator? [ type-qualifier-listopt assignment-expressionopt ]

direct-abstract-declarator? [ static type-qualifier-listopt assignment-expression ]

direct-abstract-declarator? [ type-qualifier-list static assignment-expression ]

direct-abstract-declarator? [ * ]

direct-abstract-declarator?  ( parameter-type-listopt )
*/
func (self *Parser) tryParseTypeExpression() SymbolType {
	defer self.trace("")()
	var ty SymbolType
	tok := self.peek(0)
	if tok.Kind == lexer.KEYWORD {
		if isTypeSpecifier(tok) {
			switch tok.AsString() {
			case "int":
				self.next()
				ty = &IntegerType{}
			case "float":
				self.next()
				ty = &FloatType{}
			}
		}
	}
	return ty
}

// could primary (e), (type){...}, (type)expr
func lparen_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	var (
		cast        *CastExpr
		compoundLit *CompoundLiteralExpr
		ty          SymbolType
		expr        Expression
	)

	p.match(lexer.LPAREN)
	ty = p.tryParseTypeExpression()
	if ty == nil {
		expr = p.parseExpression(0)
		//NOTE: I guess expr == nil means it's not a expression but a type
		if expr != nil {
			p.match(lexer.RPAREN)
			return expr
		} else {
			p.parseError(op.Token, "near (")
		}
	} else {
		p.match(lexer.RPAREN)
		// else it is a cast or compoundliteral, and expr should be a type
		if p.peek(0).Kind == lexer.LBRACE {
			compoundLit = &CompoundLiteralExpr{Node: Node{p.ctx}}
			compoundLit.Type = ty
			compoundLit.InitList = p.parseInitializerList()
			return compoundLit
		} else {
			cast = &CastExpr{Node: Node{p.ctx}}
			cast.Type = ty
			cast.Expr = p.parseExpression(op.NudPred)
			return cast
		}
	}

	return nil
}

func (self *Parser) parseInitializerList() *InitListExpr {
	defer self.trace("")()
	var (
		compound = false
		expr     Expression
		initList *InitListExpr
	)

	initList = &InitListExpr{Node: Node{self.ctx}}

	if self.peek(0).Kind == lexer.LBRACE {
		compound = true
		self.match(lexer.LBRACE)
	}
	oldpred := operations[lexer.COMMA].LedPred
	operations[lexer.COMMA].LedPred = -1

	if compound {
		for {
			if self.peek(0).Kind == lexer.RBRACE {
				break
			}

			expr = self.parseExpression(0)
			initList.inits = append(initList.inits, expr)
			if self.peek(0).Kind == lexer.COMMA {
				self.next()
			}
		}

		self.match(lexer.RBRACE)
	} else {
		expr = self.parseExpression(0)
		initList.inits = append(initList.inits, expr)
	}
	operations[lexer.COMMA].LedPred = oldpred

	return initList
}

// for initializer
func brace_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	return p.parseInitializerList()
}

// end of expr
func expr_led(p *Parser, lhs Expression, op *operation) Expression {
	return nil
}

// parse error
func error_led(p *Parser, lhs Expression, op *operation) Expression {
	p.parseError(op.Token, "expect an operator")
	return nil
}

func error_nud(p *Parser, op *operation) Expression {
	p.parseError(op.Token, "expect an expression")
	return nil
}

// for ID
func id_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	p.next()
	return &DeclRefExpr{Node{p.ctx}, op.Token.AsString()}
}

// for Literal (int, float, string, char...)
func literal_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	p.next()
	switch op.Kind {
	case lexer.INT_LITERAL:
		return &IntLiteralExpr{Node: Node{p.ctx}, Tok: op.Token}
	case lexer.STR_LITERAL:
		return &StringLiteralExpr{Node: Node{p.ctx}, Tok: op.Token}
	case lexer.CHAR_LITERAL:
		return &CharLiteralExpr{Node: Node{p.ctx}, Tok: op.Token}
	}
	return nil
}

func (self *Parser) parseExpression(rbp int) (ret Expression) {
	defer self.trace("")()

	if self.peek(0).Kind == lexer.SEMICOLON {
		return nil
	}

	operand := self.newOperation(self.peek(0))
	lhs := operand.nud(self, operand)

	op := self.newOperation(self.peek(0))
	for rbp < op.LedPred {
		lhs = op.led(self, lhs, op)
		op = self.newOperation(self.peek(0))
	}

	return lhs
}

func (self *Parser) PushScope() *SymbolScope {
	var scope = &SymbolScope{}
	scope.Parent = self.currentScope
	self.currentScope.Children = append(self.currentScope.Children, scope)

	self.currentScope = scope
	return scope
}

func (self *Parser) PopScope() *SymbolScope {
	if self.currentScope == self.ctx.top {
		panic("cannot pop top of the scope chain")
	}

	var ret = self.currentScope
	self.currentScope = ret.Parent
	return ret
}

func (self *Parser) AddSymbol(sym *Symbol) {
	self.currentScope.AddSymbol(sym)
}

// this is for type symbol name such as struct/enum/union/typedef
func (self *Parser) AddTypeSymbol(sym *Symbol) {
	var current = self.currentScope

done:
	for ; current != nil; current = current.Parent {
		switch current.Owner.(type) {
		case *CompoundStmt:
			break done
		case *TranslationUnit:
			break done
		}
	}
	sym.Custom = true
	current.AddSymbol(sym)
}

func (self *Parser) LookupTypeSymbol(name string) *Symbol {
	return self.currentScope.LookupSymbol(name, true)
}

func (self *Parser) LookupSymbol(name string) *Symbol {
	return self.currentScope.LookupSymbol(name, false)
}

func (self *Parser) AddUserType(st SymbolType) {
	var current = self.currentScope

done:
	for ; current != nil; current = current.Parent {
		switch current.Owner.(type) {
		case *CompoundStmt:
			break done
		case *TranslationUnit:
			break done
		}
	}

	current.RegisterUserType(st)
}

func (self *Parser) LookupUserType(name string) SymbolType {
	var current = self.currentScope

	for ; current != nil; current = current.Parent {
		if ty := current.LookupUserType(name); ty != nil {
			return ty
		}
	}
	return nil
}

// this is useless, need to trace symbol hierachy from TU
func (self *Parser) DumpSymbols() {
	var dumpSymbols func(scope *SymbolScope, level int)
	dumpSymbols = func(scope *SymbolScope, level int) {
		for _, sym := range scope.Symbols {
			fmt.Printf("%s%s\n", strings.Repeat(" ", level*2), sym.Name.AsString())
		}

		for _, sub := range scope.Children {
			dumpSymbols(sub, level+1)
		}
	}

	dumpSymbols(self.ctx.top, 0)
}

func (self *Parser) DumpAst() {
	var (
		stack  int = 0
		scope  *SymbolScope
		scopes []*SymbolScope
		clr    int
	)

	var Pop = func() *SymbolScope {
		sc := scopes[len(scopes)-1]
		scopes = scopes[:len(scopes)-1]
		return sc
	}

	var Push = func(sc *SymbolScope) {
		scopes = append(scopes, sc)
		scope = sc
	}

	var walker = struct {
		WalkTranslationUnit      func(WalkStage, *TranslationUnit)
		WalkIntLiteralExpr       func(ws WalkStage, e *IntLiteralExpr)
		WalkCharLiteralExpr      func(ws WalkStage, e *CharLiteralExpr)
		WalkStringLiteralExpr    func(ws WalkStage, e *StringLiteralExpr)
		WalkBinaryOperation      func(ws WalkStage, e *BinaryOperation)
		WalkDeclRefExpr          func(ws WalkStage, e *DeclRefExpr)
		WalkUnaryOperation       func(ws WalkStage, e *UnaryOperation)
		WalkConditionalOperation func(ws WalkStage, e *ConditionalOperation)
		WalkArraySubscriptExpr   func(ws WalkStage, e *ArraySubscriptExpr)
		WalkMemberExpr           func(ws WalkStage, e *MemberExpr)
		WalkFunctionCall         func(ws WalkStage, e *FunctionCall)
		WalkCompoundAssignExpr   func(ws WalkStage, e *CompoundAssignExpr)
		WalkCastExpr             func(ws WalkStage, e *CastExpr)
		WalkCompoundLiteralExpr  func(ws WalkStage, e *CompoundLiteralExpr)
		WalkInitListExpr         func(ws WalkStage, e *InitListExpr)
		WalkFieldDecl            func(ws WalkStage, e *FieldDecl)
		WalkRecordDecl           func(ws WalkStage, e *RecordDecl)
		WalkVariableDecl         func(ws WalkStage, e *VariableDecl)
		WalkParamDecl            func(ws WalkStage, e *ParamDecl)
		WalkFunctionDecl         func(ws WalkStage, e *FunctionDecl)
		WalkExprStmt             func(ws WalkStage, e *ExprStmt)
		WalkLabelStmt            func(ws WalkStage, e *LabelStmt)
		WalkCaseStmt             func(ws WalkStage, e *CaseStmt)
		WalkDefaultStmt          func(ws WalkStage, e *DefaultStmt)
		WalkReturnStmt           func(ws WalkStage, e *ReturnStmt)
		WalkIfStmt               func(ws WalkStage, e *IfStmt)
		WalkSwitchStmt           func(ws WalkStage, e *SwitchStmt)
		WalkWhileStmt            func(ws WalkStage, e *WhileStmt)
		WalkDoStmt               func(ws WalkStage, e *DoStmt)
		WalkDeclStmt             func(ws WalkStage, e *DeclStmt)
		WalkForStmt              func(ws WalkStage, e *ForStmt)
		WalkGotoStmt             func(ws WalkStage, e *GotoStmt)
		WalkContinueStmt         func(ws WalkStage, e *ContinueStmt)
		WalkBreakStmt            func(ws WalkStage, e *BreakStmt)
		WalkCompoundStmt         func(ws WalkStage, e *CompoundStmt)
	}{}

	var log = func(msg string) {
		if 1 == stack {
			clr = rand.Intn(200) + 50
		}
		fmt.Print(fmt.Sprintf("\033[38;5;%dm%s%s\033[00m\n", clr, strings.Repeat("  ", stack), msg))
	}

	walker.WalkTranslationUnit = func(ws WalkStage, tu *TranslationUnit) {
		if ws == WalkerPropagate {
			scope = self.ctx.top
			log("TranslationUnit")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkIntLiteralExpr = func(ws WalkStage, e *IntLiteralExpr) {
		if ws == WalkerPropagate {
			log(reflect.TypeOf(e).Elem().Name())
		}
	}

	walker.WalkCharLiteralExpr = func(ws WalkStage, e *CharLiteralExpr) {
		if ws == WalkerPropagate {
			log(e.Repr())
		}
	}
	walker.WalkStringLiteralExpr = func(ws WalkStage, e *StringLiteralExpr) {
		if ws == WalkerPropagate {
			log(e.Repr())
		}
	}

	walker.WalkBinaryOperation = func(ws WalkStage, e *BinaryOperation) {
		if ws == WalkerPropagate {
			var ty = reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%s)", ty.Name(), lexer.TokKinds[e.Op]))
			stack++
		} else {
			stack--
		}
	}

	walker.WalkDeclRefExpr = func(ws WalkStage, e *DeclRefExpr) {
		if ws == WalkerPropagate {
			log(e.Repr())
		}
	}

	walker.WalkUnaryOperation = func(ws WalkStage, e *UnaryOperation) {
		if ws == WalkerPropagate {
			var ty = reflect.TypeOf(e).Elem()
			if e.Postfix {
				log(fmt.Sprintf("%s(postfix %s)", ty.Name(), lexer.TokKinds[e.Op]))
			} else {
				log(fmt.Sprintf("%s(prefix %s)", ty.Name(), lexer.TokKinds[e.Op]))
			}
			stack++
		} else {
			stack--
		}
	}
	walker.WalkConditionalOperation = func(ws WalkStage, e *ConditionalOperation) {
		if ws == WalkerPropagate {
			log("ConditionalOperation")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkArraySubscriptExpr = func(ws WalkStage, e *ArraySubscriptExpr) {
		if ws == WalkerPropagate {
			log("ArraySubscriptExpr")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkMemberExpr = func(ws WalkStage, e *MemberExpr) {
		if ws == WalkerPropagate {
			log("MemberExpr")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkFunctionCall = func(ws WalkStage, e *FunctionCall) {
		if ws == WalkerPropagate {
			log("FunctionCall")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkCompoundAssignExpr = func(ws WalkStage, e *CompoundAssignExpr) {
		if ws == WalkerPropagate {
			var ty = reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%s)", ty.Name(), lexer.TokKinds[e.Op]))
			stack++
		} else {
			stack--
		}
	}

	walker.WalkCastExpr = func(ws WalkStage, e *CastExpr) {
		if ws == WalkerPropagate {
			log(fmt.Sprintf("CastExpr(%s)", e.Type))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkCompoundLiteralExpr = func(ws WalkStage, e *CompoundLiteralExpr) {
		if ws == WalkerPropagate {
			log(fmt.Sprintf("CompoundLiteralExpr(%s)", e.Type))
			stack++
		} else {
			stack--
		}
	}

	walker.WalkBreakStmt = func(ws WalkStage, e *BreakStmt) {
		if ws == WalkerPropagate {
			log("BreakStmt")
		}
	}

	walker.WalkContinueStmt = func(ws WalkStage, e *ContinueStmt) {
		if ws == WalkerPropagate {
			log("ContinueStmt")
		}
	}

	walker.WalkInitListExpr = func(ws WalkStage, e *InitListExpr) {
		if ws == WalkerPropagate {
			log("InitListExpr")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkFieldDecl = func(ws WalkStage, e *FieldDecl) {
		if ws == WalkerPropagate {
			log(fmt.Sprintf("FieldDecl(%s)", e.Sym))
		}
	}
	walker.WalkRecordDecl = func(ws WalkStage, e *RecordDecl) {
		if ws == WalkerPropagate {
			sym := scope.LookupSymbol(e.Sym, true)

			ty := "struct"
			if sym.Type.(*RecordType).Union {
				ty = "union"
			}

			log(fmt.Sprintf("RecordDecl(%s %s)", ty, e.Sym))
			stack++

			Push(e.Scope)
		} else {
			stack--
			scope = Pop()
		}
	}
	walker.WalkVariableDecl = func(ws WalkStage, e *VariableDecl) {
		if ws == WalkerPropagate {
			sym := scope.LookupSymbol(e.Sym, false)

			log(fmt.Sprintf("VarDecl(%s)", sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkParamDecl = func(ws WalkStage, e *ParamDecl) {
		if ws == WalkerPropagate {
			sym := scope.LookupSymbol(e.Sym, false)
			ty := reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%v)", ty.Name(), sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkFunctionDecl = func(ws WalkStage, e *FunctionDecl) {
		if ws == WalkerPropagate {
			Push(e.Scope)
			sym := scope.LookupSymbol(e.Name, false)
			log(fmt.Sprintf("FuncDecl(%v)", sym))
			stack++
		} else {
			stack--
			scope = Pop()
		}
	}
	walker.WalkExprStmt = func(ws WalkStage, e *ExprStmt) {
		if ws == WalkerPropagate {
			log("ExprStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkLabelStmt = func(ws WalkStage, e *LabelStmt) {
		if ws == WalkerPropagate {
			log(fmt.Sprintf("LabelStmt(%s)", e.Label))
			stack++
		} else {
			stack--
		}
	}

	walker.WalkCaseStmt = func(ws WalkStage, e *CaseStmt) {
		if ws == WalkerPropagate {
			log("CaseStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDefaultStmt = func(ws WalkStage, e *DefaultStmt) {
		if ws == WalkerPropagate {
			log("DefaultStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkReturnStmt = func(ws WalkStage, e *ReturnStmt) {
		if ws == WalkerPropagate {
			log("ReturnStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkSwitchStmt = func(ws WalkStage, e *SwitchStmt) {
		if ws == WalkerPropagate {
			log("SwitchStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkWhileStmt = func(ws WalkStage, e *WhileStmt) {
		if ws == WalkerPropagate {
			log("WhileStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDoStmt = func(ws WalkStage, e *DoStmt) {
		if ws == WalkerPropagate {
			log("DoStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDeclStmt = func(ws WalkStage, e *DeclStmt) {
		if ws == WalkerPropagate {
			log("DeclStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkIfStmt = func(ws WalkStage, e *IfStmt) {
		if ws == WalkerPropagate {
			log("IfStmt")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkGotoStmt = func(ws WalkStage, e *GotoStmt) {
		if ws == WalkerPropagate {
			log(fmt.Sprintf("Goto(%s)", e.Label))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkForStmt = func(ws WalkStage, e *ForStmt) {
		if ws == WalkerPropagate {

			Push(e.Scope)
			log("ForStmt")
			stack++
		} else {
			stack--
			scope = Pop()
		}
	}
	walker.WalkCompoundStmt = func(ws WalkStage, e *CompoundStmt) {
		if ws == WalkerPropagate {
			Push(e.Scope)
			log("CompoundStmt")
			stack++
		} else {
			stack--
			scope = Pop()
		}
	}

	WalkAst(self.tu, walker)
}

func (self *Parser) handlePanic(kd lexer.Kind) {
	defer self.trace("")()
	if p := recover(); p != nil {
		util.Printf(util.Parser, util.Critical, "Parse Error: %v\n", p)
		for tok := self.next(); tok.Kind != lexer.EOT && tok.Kind != kd; tok = self.next() {
		}
	}
}

func (self *Parser) trace(msg string) func() {
	var __func__ string
	if self.verbose {
		pc, _, _, _ := runtime.Caller(1)
		__func__ = runtime.FuncForPC(pc).Name()
		util.Printf(util.Parser, util.Debug, "Enter %s: %s\n", __func__, msg)
	}
	return func() {
		if self.verbose {
			util.Printf(util.Parser, util.Debug, "Exit %s: %s\n", __func__, msg)
		}
	}
}

func init() {
	util.Println(util.Parser, util.Debug, "init parser")

	storages = make(map[string]Storage)
	storages["auto"] = Auto
	storages["static"] = Static
	storages["external"] = External
	storages["register"] = Register
	storages["typedef"] = Typedef

	typeSpecifier = make(map[string]bool)
	var ts = [...]string{"void", "char", "short", "int", "long", "float",
		"double", "signed", "unsigned", "struct", "union", "enum"}
	for _, v := range ts {
		typeSpecifier[v] = true
	}

	typeQualifier = make(map[string]Qualifier)
	typeQualifier["const"] = Const
	typeQualifier["restrict"] = Restrict
	typeQualifier["volatile"] = Volatile

	operations = make(map[lexer.Kind]*operation)

	// make , right assoc, so evaluation begins from leftmost expr
	operations[lexer.COMMA] = &operation{lexer.Token{}, LeftAssoc, -1, 10, error_nud, binop_led}

	operations[lexer.ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, binop_led}
	operations[lexer.MUL_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.DIV_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.MOD_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.PLUS_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.MINUS_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.LSHIFT_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.RSHIFT_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.AND_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.OR_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}
	operations[lexer.XOR_ASSIGN] = &operation{lexer.Token{}, RightAssoc, -1, 20, error_nud, assign_led}

	//?:
	operations[lexer.QUEST] = &operation{lexer.Token{}, RightAssoc, -1, 30, error_nud, condop_led}
	operations[lexer.COLON] = &operation{lexer.Token{}, RightAssoc, -1, -1, error_nud, expr_led}

	operations[lexer.LOG_OR] = &operation{lexer.Token{}, LeftAssoc, -1, 40, error_nud, binop_led}
	operations[lexer.LOG_AND] = &operation{lexer.Token{}, LeftAssoc, -1, 50, error_nud, binop_led}

	operations[lexer.OR] = &operation{lexer.Token{}, LeftAssoc, -1, 60, error_nud, binop_led}
	operations[lexer.XOR] = &operation{lexer.Token{}, LeftAssoc, -1, 70, error_nud, binop_led}
	operations[lexer.AND] = &operation{lexer.Token{}, LeftAssoc, 150, 80, error_nud, binop_led}

	operations[lexer.EQUAL] = &operation{lexer.Token{}, LeftAssoc, -1, 90, error_nud, binop_led}
	operations[lexer.NE] = &operation{lexer.Token{}, LeftAssoc, -1, 90, error_nud, binop_led}

	// >, <, <=, >=
	operations[lexer.GREAT] = &operation{lexer.Token{}, LeftAssoc, -1, 100, error_nud, binop_led}
	operations[lexer.LESS] = &operation{lexer.Token{}, LeftAssoc, -1, 100, error_nud, binop_led}
	operations[lexer.GE] = &operation{lexer.Token{}, LeftAssoc, -1, 100, error_nud, binop_led}
	operations[lexer.LE] = &operation{lexer.Token{}, LeftAssoc, -1, 100, error_nud, binop_led}

	operations[lexer.LSHIFT] = &operation{lexer.Token{}, LeftAssoc, -1, 110, error_nud, binop_led}
	operations[lexer.RSHIFT] = &operation{lexer.Token{}, LeftAssoc, -1, 110, error_nud, binop_led}

	operations[lexer.MINUS] = &operation{lexer.Token{}, LeftAssoc, 140, 120, unaryop_nud, binop_led}
	operations[lexer.PLUS] = &operation{lexer.Token{}, LeftAssoc, 140, 120, unaryop_nud, binop_led}

	operations[lexer.MUL] = &operation{lexer.Token{}, LeftAssoc, 140, 130, unaryop_nud, binop_led}
	operations[lexer.DIV] = &operation{lexer.Token{}, LeftAssoc, -1, 130, error_nud, binop_led}
	operations[lexer.MOD] = &operation{lexer.Token{}, LeftAssoc, -1, 130, error_nud, binop_led}

	// unary !, ~
	operations[lexer.NOT] = &operation{lexer.Token{}, LeftAssoc, -1, 140, unaryop_nud, error_led}
	operations[lexer.TILDE] = &operation{lexer.Token{}, LeftAssoc, -1, 140, unaryop_nud, error_led}
	// &, *, +, - is assigned beforehand

	// NOTE: ( can appear at a lot of places: primary (expr), postfix (type){initlist}, postfix func()
	// need special take-care
	// when cast NudPred = 140
	// when primary  = 200
	// when (type) = 160
	operations[lexer.LPAREN] = &operation{lexer.Token{}, LeftAssoc, 140, 160, lparen_nud, lparen_led}
	operations[lexer.RPAREN] = &operation{lexer.Token{}, LeftAssoc, -1, -1, error_nud, expr_led}

	// prefix and postfix
	operations[lexer.INC] = &operation{lexer.Token{}, LeftAssoc, 140, 160, unaryop_nud, unaryop_led}
	operations[lexer.DEC] = &operation{lexer.Token{}, LeftAssoc, 140, 160, unaryop_nud, unaryop_led}

	operations[lexer.OPEN_BRACKET] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, array_led}
	operations[lexer.CLOSE_BRACKET] = &operation{lexer.Token{}, LeftAssoc, -1, -1, error_nud, expr_led}
	operations[lexer.DOT] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, member_led}
	operations[lexer.REFERENCE] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, member_led}

	operations[lexer.INT_LITERAL] = &operation{lexer.Token{}, NoAssoc, 200, -1, literal_nud, error_led}
	operations[lexer.STR_LITERAL] = &operation{lexer.Token{}, NoAssoc, 200, -1, literal_nud, error_led}
	operations[lexer.IDENTIFIER] = &operation{lexer.Token{}, NoAssoc, 200, -1, id_nud, error_led}

	// this is for cast-expr, compoundinitexpr
	operations[lexer.LBRACE] = &operation{lexer.Token{}, NoAssoc, 150, -1, brace_nud, expr_led}
	operations[lexer.RBRACE] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, expr_led}

	operations[lexer.SEMICOLON] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, expr_led}

	operations[lexer.ERROR] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, error_led}
}
