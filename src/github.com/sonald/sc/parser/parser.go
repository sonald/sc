package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"math/rand"
	"os"
	"reflect"
	"runtime"
	"strings"
)

//maximum lookaheads
const NR_LA = 4

type ParsingContext int

type Parser struct {
	lex          *lexer.Scanner
	tokens       [NR_LA]lexer.Token // support 4-lookahead
	cursor       int
	eot          bool // meat EOT
	ctx          *AstContext
	currentScope *SymbolScope
	tu           *TranslationUnit
	verbose      bool
}

type ParseOption struct {
	filename    string
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
	if f, err := os.Open(opts.filename); err != nil {
		util.Printf("%s\n", err.Error())
		return nil
	} else {
		self.lex = lexer.NewScanner(f)
	}

	for i := range self.tokens {
		self.tokens[i] = self.getNextToken()
	}

	self.verbose = opts.verbose

	return self.parseTU(opts)
}

// translation-unit: external-declaration+
func (self *Parser) parseTU(opts *ParseOption) Ast {
	self.tu = &TranslationUnit{}
	self.tu.filename = opts.filename
	for self.peek(0).Kind != lexer.EOT {
		self.parseExternalDecl(opts)
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
	panic(fmt.Sprintf("tok %s(%s), %s", lexer.TokKinds[tok.Kind], tok.AsString(), msg))
}

func (self *Parser) parseTypeDecl(opts *ParseOption, sym *Symbol) {
	defer self.trace("")()

	var ty SymbolType

	for {
		tok := self.peek(0)
		if tok.Kind == lexer.KEYWORD {
			self.next()
			if isStorageClass(tok) {
				if sym.Storage == NilStorage {
					sym.Storage = storages[tok.AsString()]
				} else {
					self.parseError(tok, "multiple storage class specified")
				}
			} else if isTypeSpecifier(tok) {
				//FIXME: handle multiple typespecifier
				if sym.Type != nil {
					if _, qualified := sym.Type.(*QualifiedType); !qualified {
						self.parseError(tok, "multiple type specifier")
					}
				}
				switch tok.AsString() {
				case "int":
					ty = &IntegerType{}
				case "float":
					ty = &FloatType{}
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
				sym.Type = &QualifiedType{Base: sym.Type, Qualifier: typeQualifier[tok.AsString()]}
				//self.parseError(tok, "multiple type qualifier specified")
			} else if tok.AsString() == "inline" {
				//ignore now
			} else {
				self.parseError(tok, "invalid declaration specifier")
			}
			//FIXME: handle usertype by typedef, struct, union
		} else {
			break
		}
	}

	util.Printf("parsed type template %v", sym)
}

func (self *Parser) parseFunctionParams(opts *ParseOption, decl *FunctionDecl) {
	defer self.trace("")()

	funcSym := self.currentScope.LookupSymbol(decl.Name)
	var ty = funcSym.Type.(*Function)
	for {
		if self.peek(0).Kind == lexer.RPAREN {
			break
		}

		var tmpl = &Symbol{}
		self.parseTypeDecl(opts, tmpl)
		if arg := self.parseDeclarator(opts, tmpl); arg == nil {
			break
		} else {
			switch arg.(type) {
			case *VariableDecl:
				var pd = &ParamDecl{decl.Node, arg.(*VariableDecl).Sym}
				decl.Args = append(decl.Args, pd)

				pty := decl.Scope.LookupSymbol(pd.Sym)
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
func (self *Parser) parseDeclarator(opts *ParseOption, sym *Symbol) Ast {
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
		self.parseFunctionParams(opts, fdecl)
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
			decl.(*VariableDecl).init = self.parseInitializerList(opts)
		default:
			self.parseError(self.peek(0), "Initializer is not allowed here")
		}
	}
	return decl
}

func (self *Parser) parseExternalDecl(opts *ParseOption) Ast {
	defer self.trace("")()

	var tmpl = &Symbol{}
	self.parseTypeDecl(opts, tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(opts, tmpl); decl == nil {
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
					fdecl.Body = self.parseCompoundStmt(opts)
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

func (self *Parser) parseCompoundStmt(opts *ParseOption) *CompoundStmt {
	defer self.trace("")()
	var scope = self.PushScope()
	var compound = &CompoundStmt{Node: Node{self.ctx}, Scope: scope}

	self.match(lexer.LBRACE)

	for {
		if self.peek(0).Kind == lexer.RBRACE {
			break
		}
		compound.Stmts = append(compound.Stmts, self.parseStatement(opts))
	}
	self.match(lexer.RBRACE)
	self.PopScope()
	return compound
}

func (self *Parser) parseStatement(opts *ParseOption) Statement {
	defer self.trace("")()
	tok := self.peek(0)

	var stmt Statement
	// all normal statements

	// else
	switch tok.Kind {
	case lexer.KEYWORD:
		//FIXME: handle typedef usertype decl
		if isStorageClass(tok) || isTypeQualifier(tok) || isTypeSpecifier(tok) {
			stmt = self.parseDeclStatement(opts)
		} else {
			stmt = self.parseExprStatement(opts)
		}

	default:
		stmt = self.parseExprStatement(opts)
	}

	util.Printf("parsed %s\n", stmt.Repr())
	return stmt
}

func (self *Parser) parseDeclStatement(opts *ParseOption) *DeclStmt {
	defer self.trace("")()

	var declStmt = &DeclStmt{Node: Node{self.ctx}}

	var tmpl = &Symbol{}
	self.parseTypeDecl(opts, tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(opts, tmpl); decl == nil {
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

func (self *Parser) parseExprStatement(opts *ParseOption) Expression {
	defer self.trace("")()
	expr := self.parseExpression(opts, 0)
	self.match(lexer.SEMICOLON)

	return expr
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

// alloc new operation by copying specific template
func newOperation(tok lexer.Token) *operation {
	var op = *operations[tok.Kind]
	op.Token = tok
	return &op
}

// for binary op
// handle comma carefully
func binop_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()

	p.next() // eat op
	rhs := p.parseExpression(nil, op.LedPred)

	var expr = &BinaryOperation{Node{p.ctx}, op.Token.Kind, lhs, rhs}
	util.Printf("parsed %v", expr.Repr())
	return expr
}

func assign_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()

	p.next() // eat op
	rhs := p.parseExpression(nil, op.LedPred)

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
	expr.True = p.parseExpression(nil, op.LedPred)
	p.match(lexer.COLON) // eat :

	expr.False = p.parseExpression(nil, op.LedPred)

	util.Printf("parsed %v", expr.Repr())
	return expr
}

// for unary (including prefix)
func unaryop_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	p.next()
	var expr = p.parseExpression(nil, op.NudPred)
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
	expr.Member = p.parseExpression(nil, op.LedPred)
	return expr
}

// e1[e2]
func array_led(p *Parser, lhs Expression, op *operation) Expression {
	defer p.trace("")()
	p.match(lexer.OPEN_BRACKET)

	var expr = &ArraySubscriptExpr{Node: Node{p.ctx}}
	expr.Target = lhs
	expr.Sub = p.parseExpression(nil, op.LedPred)
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

		expr.Args = append(expr.Args, p.parseExpression(nil, 0))
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
		expr = p.parseExpression(nil, 0)
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
			compoundLit.InitList = p.parseInitializerList(nil)
			return compoundLit
		} else {
			cast = &CastExpr{Node: Node{p.ctx}}
			cast.Type = ty
			cast.Expr = p.parseExpression(nil, op.NudPred)
			return cast
		}
	}

	return nil
}

func (self *Parser) parseInitializerList(opts *ParseOption) *InitListExpr {
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

			expr = self.parseExpression(opts, 0)
			initList.inits = append(initList.inits, expr)
			if self.peek(0).Kind == lexer.COMMA {
				self.next()
			}
		}

		self.match(lexer.RBRACE)
	} else {
		expr = self.parseExpression(opts, 0)
		initList.inits = append(initList.inits, expr)
	}
	operations[lexer.COMMA].LedPred = oldpred

	return initList
}

// for initializer
func brace_nud(p *Parser, op *operation) Expression {
	defer p.trace("")()
	return p.parseInitializerList(nil)
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

func (self *Parser) parseExpression(opts *ParseOption, rbp int) Expression {
	defer self.trace("")()

	if self.peek(0).Kind == lexer.SEMICOLON {
		return nil
	}

	operand := newOperation(self.peek(0))
	lhs := operand.nud(self, operand)

	op := newOperation(self.peek(0))
	for rbp < op.LedPred {
		lhs = op.led(self, lhs, op)
		op = newOperation(self.peek(0))
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

func (self *Parser) LookupSymbol(name string) *Symbol {
	return self.currentScope.LookupSymbol(name)
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
	var top Ast = self.tu
	var stack int = 0
	var scope *SymbolScope

	fmt.Println("DumpAst")
	var visit func(Ast)
	var log = func(msg string) {
		var clr = rand.Intn(200) + 50
		fmt.Print(fmt.Sprintf("\033[38;5;%dm%s%s\033[00m\n", clr, strings.Repeat("  ", stack), msg))
	}

	visit = func(ast Ast) {
		switch ast.(type) {
		case *TranslationUnit:
			tu := ast.(*TranslationUnit)
			scope = self.ctx.top
			stack++
			for _, d := range tu.funcDecls {
				visit(d)
			}
			stack--

			stack++
			for _, d := range tu.varDecls {
				visit(d)
			}
			stack--

		case *IntLiteralExpr:
			e := ast.(*IntLiteralExpr)
			log(e.Repr())

		case *CharLiteralExpr:
			e := ast.(*CharLiteralExpr)
			log(e.Repr())

		case *StringLiteralExpr:
			e := ast.(*StringLiteralExpr)
			log(e.Repr())

		case *BinaryOperation:
			var (
				e  = ast.(*BinaryOperation)
				ty = reflect.TypeOf(e).Elem()
			)
			log(fmt.Sprintf("%s(%s)", ty.Name(), lexer.TokKinds[e.Op]))
			stack++
			visit(e.LHS)
			visit(e.RHS)
			stack--

		case *DeclRefExpr:
			e := ast.(*DeclRefExpr)
			log(e.Repr())

		case *UnaryOperation:
			var (
				e  = ast.(*UnaryOperation)
				ty = reflect.TypeOf(e).Elem()
			)
			if e.Postfix {
				log(fmt.Sprintf("%s(postfix %s)", ty.Name(), lexer.TokKinds[e.Op]))
			} else {
				log(fmt.Sprintf("%s(prefix %s)", ty.Name(), lexer.TokKinds[e.Op]))
			}
			stack++
			visit(e.expr)
			stack--

		case *ConditionalOperation:
			e := ast.(*ConditionalOperation)
			log("ConditionalOperation")
			stack++
			visit(e.Cond)
			visit(e.True)
			visit(e.False)
			stack--

		case *ArraySubscriptExpr:
			e := ast.(*ArraySubscriptExpr)
			log("ArraySubscriptExpr")
			stack++
			visit(e.Target)
			visit(e.Sub)
			stack--

		case *MemberExpr:
			e := ast.(*MemberExpr)
			log("MemberExpr")
			stack++
			visit(e.Target)
			visit(e.Member)
			stack--

		case *FunctionCall:
			e := ast.(*FunctionCall)
			log("FunctionCall")
			stack++
			visit(e.Func)
			for _, arg := range e.Args {
				visit(arg)
			}
			stack--

		case *CompoundAssignExpr:
			var (
				e  = ast.(*CompoundAssignExpr)
				ty = reflect.TypeOf(e).Elem()
			)
			log(fmt.Sprintf("%s(%s)", ty.Name(), lexer.TokKinds[e.Op]))
			stack++
			visit(e.LHS)
			visit(e.RHS)
			stack--

		case *CastExpr:
			e := ast.(*CastExpr)
			log(fmt.Sprintf("CastExpr(%s)", e.Type))
			stack++
			visit(e.Expr)
			stack--

		case *CompoundLiteralExpr:
			e := ast.(*CompoundLiteralExpr)
			log(fmt.Sprintf("CompoundLiteralExpr(%s)", e.Type))
			stack++
			visit(e.InitList)
			stack--

		case *InitListExpr:
			e := ast.(*InitListExpr)
			log("InitListExpr")
			stack++
			for _, init := range e.inits {
				visit(init)
			}
			stack--

		case *VariableDecl:
			e := ast.(*VariableDecl)
			sym := scope.LookupSymbol(e.Sym)

			log(fmt.Sprintf("VarDecl(%s)", sym))
			if e.init != nil {
				stack++
				visit(e.init)
				stack--
			}

		case *Initializer:
		case *ParamDecl:
			e := ast.(*ParamDecl)
			sym := scope.LookupSymbol(e.Sym)
			ty := reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%v)", ty.Name(), sym))

		case *FunctionDecl:
			e := ast.(*FunctionDecl)
			scope = e.Scope
			sym := scope.LookupSymbol(e.Name)
			log(fmt.Sprintf("FuncDecl(%v)", sym))

			stack++
			for _, arg := range e.Args {
				visit(arg)
			}
			stack--

			if e.Body != nil {
				visit(e.Body)
			}

		case *LabelStmt:
		case *CaseStmt:
		case *DefaultStmt:
		case *ReturnStmt:
		case *IfStmt:
		case *SwitchStmt:
		case *WhileStmt:
		case *DoStmt:
		case *DeclStmt:
			e := ast.(*DeclStmt)
			log("DeclStmt")
			stack++
			for _, stmt := range e.Decls {
				visit(stmt)
			}
			stack--

		case *ForStmt:
		case *GotoStmt:
		case *ContinueStmt:
		case *BreakStmt:
		case *CompoundStmt:
			e := ast.(*CompoundStmt)
			scope = e.Scope
			log("CompoundStmt")
			stack++
			for _, stmt := range e.Stmts {
				visit(stmt)
			}
			stack--

		default:
			break
		}
	}

	visit(top)
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

}
