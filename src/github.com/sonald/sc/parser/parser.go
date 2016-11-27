package parser

import (
	"fmt"
	"github.com/sonald/sc/ast"
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
	ctx             *ast.AstContext
	currentScope    *ast.SymbolScope
	tu              *ast.TranslationUnit
	effectiveParent ast.Ast // This is a bad name, it is used for ast.RecordDecl parsing
	verbose         bool
	Reports         []*ast.Report
}

type ParseOption struct {
	Filename string
	Reader   io.Reader
	Verbose  bool // log call trace
}

func NewParser() *Parser {
	p := &Parser{}

	var top = ast.SymbolScope{}
	p.ctx = &ast.AstContext{Top: &top}
	p.currentScope = &top

	return p
}

func (self *Parser) peek(n int) lexer.Token {
	if n >= NR_LA || n < 0 {
		panic(fmt.Sprintf("only maximum of %d tokens lookahead supported", NR_LA))
	}

	tok := self.tokens[n]
	util.Printf(util.Parser, util.Debug, "peek %s(%s)", lexer.TokKinds[tok.Kind], tok.AsString())
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

func (self *Parser) makeNode(tk lexer.Token) ast.Node {
	return ast.Node{Ctx: self.ctx, Start: tk}
}

// the only entry
func (self *Parser) Parse(opts *ParseOption) ast.Ast {
	self.lex = lexer.NewScanner(opts.Reader)
	for i := range self.tokens {
		self.tokens[i] = self.getNextToken()
	}

	self.verbose = opts.Verbose

	var tu = self.parseTU(opts)

	defer func() {
		for _, r := range self.Reports {
			util.Printf(util.Parser, util.Critical, "Error: %v",
				fmt.Sprintf("%s(%s) %d:%d, %s", lexer.TokKinds[r.Token.Kind], r.AsString(),
					r.Line, r.Column, r.Desc))
		}
	}()
	return tu
}

// translation-unit: external-declaration+
func (self *Parser) parseTU(opts *ParseOption) ast.Ast {
	defer self.trace("")()

	self.tu = &ast.TranslationUnit{}
	self.tu.Filename = opts.Filename
	self.effectiveParent = self.tu
	self.ctx.Top.Owner = self.tu
	self.tu.Ctx = self.ctx

	for self.peek(0).Kind != lexer.EOT {
		self.parseExternalDecl()
	}
	return self.tu
}

func (self *Parser) parseError(tok lexer.Token, msg string) {
	var r = ast.MakeReport(ast.Error, tok, msg)
	self.Reports = append(self.Reports, r)
	panic(r)
}

func (self *Parser) parseTypeDecl(sym *ast.Symbol) (isTypedef bool) {
	defer self.trace("")()
	var (
		ty   ast.SymbolType
		err1 = "%s can not combine with %s type specifier"
		err2 = "%s is invalid type"
		err3 = "invalid combination of type specifiers"
		// Location in every key should be in scan order, so I can find the previous conflicting
		parts = make(map[string][]lexer.Location)
	)

	var doCheckError = func(tok lexer.Token) {
		var (
			l        = len(parts["long"])
			i        = len(parts["int"])
			s        = len(parts["short"])
			c        = len(parts["char"])
			unsigned = len(parts["unsigned"])
			signed   = len(parts["signed"])
		)
		if l > 0 {
			if s > 0 {
				self.parseError(tok, fmt.Sprintf(err1, "long", "short"))
			} else if c > 0 {
				self.parseError(tok, fmt.Sprintf(err2, "long char"))
			}

			if l > 2 {
				self.parseError(tok, fmt.Sprintf(err1, "long", "long long"))
			}
		} else if i > 0 {
			if c > 0 {
				self.parseError(tok, fmt.Sprintf(err1, "char", "int"))
			} else if s > 1 {
				// report duplicaton
			} else if i > 1 {
				self.parseError(tok, fmt.Sprintf(err1, "int", "int"))
			}
		} else if s > 0 {
			if c > 0 {
				self.parseError(tok, fmt.Sprintf(err1, "char", "short"))
			}
		}

		if unsigned > 0 {
			if signed > 0 {
				self.parseError(tok, fmt.Sprintf(err1, "signed", "unsigned"))
			}
		}
	}

	for {
		tok := self.peek(0)
		if tok.Kind == lexer.KEYWORD {
			if ast.IsStorageClass(tok) {
				self.next()
				if sym.Storage == ast.NilStorage {
					sym.Storage = ast.Storages[tok.AsString()]
					if sym.Storage == ast.Typedef {
						isTypedef = true
						util.Printf(util.Parser, util.Critical, "this is a typedefing")
					}
				} else {
					self.parseError(tok, "multiple storage class specified")
				}
			} else if ast.IsTypeSpecifier(tok) {
				ts := tok.AsString()
				switch ts {
				case "union", "struct":
					if ty != nil {
						self.parseError(tok, err3)
					}
					ty = self.parseRecordType()
				case "enum":
					if ty != nil {
						self.parseError(tok, err3)
					}
					ty = self.parseEnumType()

				default:
					self.next()
					parts[ts] = append(parts[ts], tok.Location)
					if ty != nil {
						self.parseError(tok, err3)
					}
					switch ts {
					case "int", "long", "char", "short", "unsigned", "signed":
						break
					case "void":
						ty = &ast.VoidType{}
					case "float":
						ty = &ast.FloatType{}
					case "double":
						ty = &ast.DoubleType{}
					default:
						self.parseError(tok, "unknown type specifier")
					}
				}
				doCheckError(tok)

			} else if ast.IsTypeQualifier(tok) {
				self.next()
				sym.Type = &ast.QualifiedType{Base: sym.Type, Qualifier: ast.TypeQualifier[tok.AsString()]}
			} else if tok.AsString() == "inline" {
				self.next()
				//FIXME: ignore now
			} else {
				self.parseError(tok, "invalid declaration specifier")
			}
		} else if tok.Kind == lexer.IDENTIFIER {
			//TODO: make doCheckError check user type
			util.Printf("looking up user type %s", tok.AsString())
			if uty := self.LookupTypedef(tok.AsString()); uty != nil {
				util.Printf("found usertype %s", tok.AsString())
				if ty != nil {
					self.parseError(tok, err3)
				}
				ty = uty
				self.next()
			} else {
				break
			}

		} else {
			break
		}
	}

	if ty == nil {
		var (
			l        = len(parts["long"])
			i        = len(parts["int"])
			s        = len(parts["short"])
			unsigned = len(parts["unsigned"])
		)
		ity := &ast.IntegerType{}
		if l > 0 {
			if l >= 2 {
				ity.Kind = "long long"
			} else {
				ity.Kind = "long"
			}
		} else if i > 0 {
			if s > 0 {
				ity.Kind = "short"
			} else {
				ity.Kind = "int"
			}
		} else if s > 0 {
			ity.Kind = "short"
		} else {
			ity.Kind = "char"
		}

		if unsigned > 0 {
			ity.Unsigned = true
		}
		ty = ity
	}

	if sym.Type == nil {
		sym.Type = ty
	} else {
		var qty = sym.Type.(*ast.QualifiedType)
		for qty.Base != nil {
			qty = qty.Base.(*ast.QualifiedType)
		}
		qty.Base = ty
	}

	util.Printf("parsed type template %v", sym)
	return
}

func (self *Parser) parseFunctionParams(decl *ast.FunctionDecl, ty *ast.Function) {
	defer self.trace("")()

	for {
		if self.peek(0).Kind == lexer.RPAREN {
			break
		}

		if self.peek(0).Kind == lexer.ELLIPSIS {
			if self.peek(1).Kind != lexer.RPAREN {
				self.parseError(self.peek(0), "ellipsis should be the last arg of varidic function")
			}
			self.next()
			decl.IsVariadic = true
			ty.IsVariadic = true
			continue
		}

		var tmpl = &ast.Symbol{}
		if isTypedef := self.parseTypeDecl(tmpl); isTypedef {
			self.parseError(self.peek(0), "typedef is not allowed in function param")
		}
		if arg := self.parseDeclarator(tmpl); arg == nil {
			break
		} else {
			switch arg.(type) {
			case *ast.VariableDecl:
				var pd = &ast.ParamDecl{decl.Node, arg.(*ast.VariableDecl).Sym}
				decl.Args = append(decl.Args, pd)

				pty := decl.Scope.LookupSymbol(pd.Sym, ast.OrdinaryNS)
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

func (self *Parser) parseFunctionParamTypes(ty *ast.Function) {
	defer self.trace("")()

	for {
		if self.peek(0).Kind == lexer.RPAREN {
			break
		}

		if self.peek(0).Kind == lexer.ELLIPSIS {
			if self.peek(1).Kind != lexer.RPAREN {
				self.parseError(self.peek(0), "ellipsis should be the last arg of varidic function")
			}
			self.next()
			ty.IsVariadic = true
			continue
		}

		var tmpl = &ast.Symbol{}
		if isTypedef := self.parseTypeDecl(tmpl); isTypedef {
			self.parseError(self.peek(0), "typedef is not allowed in function param")
		}
		if arg := self.parseDeclarator(tmpl); arg == nil {
			break
		} else {
			switch arg.(type) {
			case *ast.VariableDecl:
				var pd = arg.(*ast.VariableDecl).Sym
				pty := self.LookupSymbol(pd, ast.OrdinaryNS)
				ty.Args = append(ty.Args, pty.Type)
				util.Printf("parsed arg type %v", pty.Type)
			default:
				self.parseError(self.peek(0), "invalid parameter declaration")
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}
}

func (self *Parser) parseDeclarator(tmpl *ast.Symbol) ast.Ast {
	defer self.trace("")()
	type Partial struct {
		ty   ast.SymbolType
		hole *ast.SymbolType
	}
	var (
		parseDeclaratorHelper func() Partial
		decl                  ast.Ast
		id                    *lexer.Token
		idLevel               = 0
		finalSym              = ast.Symbol{Storage: tmpl.Storage}
		nested                = 0 // nested level
		isTypedef             = tmpl.Storage == ast.Typedef
		refTok                = self.peek(0)
	)

	//FIXME: support const-expr
	var parseArray = func() Partial {
		defer self.trace("")()
		var (
			partial = Partial{}
			aty     = &ast.Array{}
		)

		partial.ty = aty
		partial.hole = &aty.ElemType

		for {
			self.match(lexer.OPEN_BRACKET)
			aty.Level++
			if tok := self.peek(0); tok.Kind == lexer.CLOSE_BRACKET {
				// NOTE: I use -1 means don't know
				ile := &ast.IntLiteralExpr{Node: self.makeNode(tok), Tok: lexer.MakeToken(lexer.INT_LITERAL, "-1")}
				aty.LenExprs = append(aty.LenExprs, ile)
			} else {
				aty.LenExprs = append(aty.LenExprs, self.parseExpression(0))
			}
			self.match(lexer.CLOSE_BRACKET)

			if self.peek(0).Kind != lexer.OPEN_BRACKET {
				break
			}
		}

		return partial
	}

	parseDeclaratorHelper = func() Partial {
		nested++
		defer self.trace(fmt.Sprintf("nested level %d", nested))()

		var basePartial Partial
		var nestedPartial Partial
		if nested == 1 {
			basePartial.ty = tmpl.Type
		}

		tok := self.peek(0)
		switch tok.Kind {
		case lexer.MUL:
			self.next()
			basePartial = Partial{&ast.Pointer{}, nil}
			basePartial.hole = &basePartial.ty.(*ast.Pointer).Source
			var baseType = basePartial.ty
			for {
				if forward := self.peek(0); ast.IsTypeQualifier(forward) {
					self.next()
					baseType = &ast.QualifiedType{Base: baseType, Qualifier: ast.TypeQualifier[forward.AsString()]}

				} else if forward.Kind == lexer.MUL {
					self.next()
					baseType = &ast.Pointer{baseType}
				} else {
					break
				}
			}
			basePartial.ty = baseType
		}

		if tok := self.peek(0); tok.Kind == lexer.LPAREN {
			self.match(lexer.LPAREN)
			nestedPartial = parseDeclaratorHelper()
			util.Printf(util.Parser, util.Verbose, "level %d: -> nested %v\n", nested, nestedPartial.ty)
			self.match(lexer.RPAREN)
		} else if tok.Kind == lexer.IDENTIFIER {
			self.next()
			//TODO: assert id == nil
			id = &tok
			idLevel = nested
			finalSym.Name = *id
			finalSym.NS = ast.OrdinaryNS
			self.AddSymbol(&finalSym)
		}

		switch self.peek(0).Kind {
		case lexer.OPEN_BRACKET:
			var pt = parseArray()
			util.Printf(util.Parser, util.Verbose, "level %d: -> array %v %v\n", nested, pt.ty, pt.hole)
			if basePartial.ty != nil {
				*pt.hole = basePartial.ty
				pt.hole = basePartial.hole
			}

			if nestedPartial.ty != nil {
				*nestedPartial.hole = pt.ty
				nestedPartial.hole = pt.hole
				basePartial = nestedPartial
			} else {
				basePartial = pt
			}

			if nested == 1 && id != nil {
				if isTypedef {
					decl = &ast.TypedefDecl{Node: self.makeNode(*id), Sym: id.AsString()}
				} else {
					decl = &ast.VariableDecl{Node: self.makeNode(*id), Sym: id.AsString()}
				}
			}

		case lexer.LPAREN: // func
			self.match(lexer.LPAREN)
			var pt = Partial{}
			pt.ty = &ast.Function{Return: basePartial.ty}
			if basePartial.ty != nil {
				pt.hole = basePartial.hole
			}

			if nested == 1 && id != nil && idLevel == nested {
				var fdecl = &ast.FunctionDecl{Node: self.makeNode(*id)}
				decl = fdecl
				fdecl.Name = id.AsString()
				// when found definition of func, we need to chain fdecl.Scope with body
				fdecl.Scope = self.PushScope()
				fdecl.Scope.Owner = fdecl
				self.parseFunctionParams(fdecl, pt.ty.(*ast.Function))
				self.PopScope()

			} else {
				// this is just a temp scope to capture params
				if nested == 1 && id != nil {
					if isTypedef {
						decl = &ast.TypedefDecl{Node: self.makeNode(*id), Sym: id.AsString()}
					} else {
						decl = &ast.VariableDecl{Node: self.makeNode(*id), Sym: id.AsString()}
					}
				}
				self.PushScope()
				self.parseFunctionParamTypes(pt.ty.(*ast.Function))
				self.PopScope()
			}
			self.match(lexer.RPAREN)

			if nestedPartial.ty != nil {
				*nestedPartial.hole = pt.ty
				nestedPartial.hole = pt.hole
				basePartial = nestedPartial
			} else {
				basePartial = pt
			}

		default:
			if id != nil {
				if isTypedef {
					decl = &ast.TypedefDecl{Node: self.makeNode(*id), Sym: id.AsString()}
				} else {
					decl = &ast.VariableDecl{Node: self.makeNode(*id), Sym: id.AsString()}
				}
			}
		}

		if self.peek(0).Kind == lexer.ASSIGN {
			// parse initializer or assignment-expr
			self.next()
			switch decl.(type) {
			case *ast.VariableDecl:
				if self.peek(0).Kind == lexer.LBRACE {
					decl.(*ast.VariableDecl).Init = self.parseInitializerList()
				} else {
					oldpred := operations[lexer.COMMA].LedPred
					operations[lexer.COMMA].LedPred = -1
					decl.(*ast.VariableDecl).Init = self.parseExpression(0)
					operations[lexer.COMMA].LedPred = oldpred
				}
			default:
				self.parseError(self.peek(0), "Initializer is not allowed here (only variables can be initialized)")
			}
		}

		util.Printf(util.Parser, util.Verbose, "level %d: -> %v", nested, basePartial.ty)
		nested--
		return basePartial
	}

	var pt = parseDeclaratorHelper()
	if pt.hole != nil {
		*pt.hole = tmpl.Type
	}
	finalSym.Type = pt.ty

	if decl == nil && id == nil && pt.ty != nil {
		// this happens if we are parsing types only (such as func params)
		// so make a dummy decl
		finalSym.Name = lexer.MakeToken(lexer.IDENTIFIER, ast.NextDummyVariableName())
		decl = &ast.VariableDecl{Node: self.makeNode(refTok), Sym: finalSym.Name.AsString()}
		self.AddSymbol(&finalSym)
	}

	if isTypedef {
		finalSym.Type = &ast.UserType{id.AsString(), finalSym.Type}
		self.AddNamedType(finalSym.Type)
	}

	util.Printf(util.Parser, util.Verbose, "parsed %v %v", finalSym.Name.AsString(), finalSym.Type)
	return decl
}

func (self *Parser) parseEnumType() ast.SymbolType {
	defer self.trace("")()
	var (
		enumDecl  = &ast.EnumDecl{Node: self.makeNode(self.peek(0))}
		ret       = &ast.EnumType{}
		enumSym   = &ast.Symbol{}
		tok       lexer.Token
		isForward bool
	)

	self.next() // eat enum

	if tok = self.peek(0); tok.Kind == lexer.IDENTIFIER {
		self.next()
		ret.Name = tok.AsString()
		enumSym.Name = tok
		if ty := self.LookupNamedType(ret.Name, ast.TagNS); ty != nil {
			var decls []ast.Statement

			switch self.effectiveParent.(type) {
			case *ast.DeclStmt:
				decls = self.effectiveParent.(*ast.DeclStmt).Decls
			case *ast.TranslationUnit:
				decls = self.tu.Decls
			default:
				return ty
			}

			if next := self.peek(0).Kind; next != lexer.SEMICOLON && next != lexer.LBRACE {
				return ty
			}
			for _, decl := range decls {
				if ed, ok := decl.(*ast.EnumDecl); ok && ed.Sym == ret.Name && !ed.IsDefinition {
					enumSym = self.LookupSymbol(ret.Name, ast.TagNS)
					ret = ty.(*ast.EnumType)
					isForward = true
					enumDecl.Prev = ed
					break
				}
			}

			if !isForward {
				return ty
			}
		}
	} else {
		ret.Name = ast.NextAnonyEnumName()
	}

	enumSym.Type = ret
	enumDecl.Sym = ret.Name
	enumSym.NS = ast.TagNS

	defer func() {
		if p := recover(); p == nil {
			if ds, ok := self.effectiveParent.(*ast.DeclStmt); ok {
				ds.Decls = append(ds.Decls, enumDecl)
			} else {
				self.tu.Decls = append(self.tu.Decls, enumDecl)
			}
		} else {
			panic(p) //propagate
		}
	}()

	if !isForward {
		self.AddNamedType(ret)
		self.AddSymbol(enumSym)
	}

	if self.peek(0).Kind == lexer.SEMICOLON {
		//forward declaration
		return ret
	}

	self.match(lexer.LBRACE)
	enumDecl.IsDefinition = true

	for {
		if self.peek(0).Kind == lexer.RBRACE {
			break
		}

		var (
			e  = &ast.EnumeratorDecl{Node: enumDecl.Node}
			es = &ast.Symbol{}
			et = &ast.EnumeratorType{}
		)
		tok = self.next()
		if tok.Kind != lexer.IDENTIFIER {
			self.parseError(tok, "need a valid enumerator constant")
		}

		et.Name = tok.AsString()
		es.Type = et
		es.Name = tok
		//FIXME: do check if redeclaration happens
		self.AddNamedType(et)
		es.NS = ast.OrdinaryNS
		self.AddSymbol(es)

		e.Sym = et.Name
		e.Loc = tok.Location

		if self.peek(0).Kind == lexer.ASSIGN {
			self.next()
			oldpred := operations[lexer.COMMA].LedPred
			operations[lexer.COMMA].LedPred = -1
			e.Value = self.parseExpression(0)
			operations[lexer.COMMA].LedPred = oldpred
		}

		enumDecl.List = append(enumDecl.List, e)
		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}

	self.match(lexer.RBRACE)

	util.Printf("parsed ast.EnumType: %v", ret)

	return ret
}

func (self *Parser) parseRecordType() ast.SymbolType {
	defer self.trace("")()
	var (
		recDecl   = &ast.RecordDecl{Node: self.makeNode(self.peek(0))}
		recSym    = &ast.Symbol{}
		ret       = &ast.RecordType{}
		tok       lexer.Token
		isForward bool
	)

	tok = self.next()
	ret.Union = tok.AsString() == "union"

	if tok = self.next(); tok.Kind == lexer.IDENTIFIER {
		ret.Name = tok.AsString()
		recSym.Name = tok
		if ty := self.LookupNamedType(ret.Name, ast.TagNS); ty != nil {
			var decls []ast.Statement
			switch self.effectiveParent.(type) {
			case *ast.DeclStmt:
				decls = self.effectiveParent.(*ast.DeclStmt).Decls
			case *ast.TranslationUnit:
				decls = self.tu.Decls
			default:
				return ty
			}

			if next := self.peek(0).Kind; next != lexer.SEMICOLON && next != lexer.LBRACE {
				return ty
			}
			for _, decl := range decls {
				if rd, ok := decl.(*ast.RecordDecl); ok && rd.Sym == ret.Name && !rd.IsDefinition {
					recSym = self.LookupSymbol(ret.Name, ast.TagNS)
					ret = ty.(*ast.RecordType)
					isForward = true
					recDecl.Scope = rd.Scope
					recDecl.Prev = rd
					break
				}
			}

			if !isForward {
				return ty
			}
		}
	} else {
		ret.Name = ast.NextAnonyRecordName()
	}

	recSym.Type = ret
	recSym.NS = ast.TagNS
	recDecl.Sym = ret.Name
	if ds, ok := self.effectiveParent.(*ast.DeclStmt); ok {
		ds.Decls = append(ds.Decls, recDecl)
	} else {
		self.tu.Decls = append(self.tu.Decls, recDecl)
	}

	defer func() {
		self.PopScope()
		if p := recover(); p != nil {
			// if this is top level of record decl, skip it and continue
			if _, ok := self.currentScope.Owner.(*ast.RecordDecl); !ok {
				util.Printf(util.Parser, util.Warning, p)
				for tok := self.next(); tok.Kind != lexer.EOT && tok.Kind != lexer.RBRACE; tok = self.next() {
				}
				self.mayIgnore(lexer.SEMICOLON)
			}
			panic(p) //propagate
		}
	}()

	//NOTE: we register here so that pointer of this type can be used as field type
	//FIXME: if parse failed, need to deregister it

	if !isForward {
		self.AddNamedType(ret)
		self.AddSymbol(recSym)
		recDecl.Scope = self.PushScope()
	} else {
		self.currentScope = recDecl.Scope
	}
	recDecl.Scope.Owner = recDecl //NOTE: this changes Owner to last definition of the same record
	if nt := self.peek(0); nt.Kind == lexer.SEMICOLON {
		//forward declaration
		return ret
	} else if nt.Kind != lexer.LBRACE {
		// consider it as forward decl (e.g inside a typedefing)
		return ret
	}

	self.match(lexer.LBRACE)
	recDecl.IsDefinition = true

	for {
		if self.peek(0).Kind == lexer.RBRACE {
			break
		}

		var tmplSym = &ast.Symbol{}
		var loc = self.peek(0).Location

		if isTypedef := self.parseTypeDecl(tmplSym); isTypedef {
			self.parseError(self.peek(0), "typedef is not allowed in record")
		}

		util.Printf("parsed field type template %v", tmplSym)

		for {
			if self.peek(0).Kind == lexer.SEMICOLON {
				self.next()
				break
			}

			var fd = &ast.FieldDecl{Node: self.makeNode(self.peek(0))}
			var ft = &ast.FieldType{}

			if self.peek(0).Kind != lexer.COLON {
				// FIXME: parseDeclarator will add new symbol into current scope,
				// which will pollute scoping rule
				var decl = self.parseDeclarator(tmplSym)
				switch decl.(type) {
				case *ast.VariableDecl:
					var vd = decl.(*ast.VariableDecl)
					var vs = self.LookupSymbol(vd.Sym, ast.OrdinaryNS)

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
				fd.Sym = ast.NextAnonyFieldName(recDecl.Sym)
				recDecl.Fields = append(recDecl.Fields, fd)

				ft.Base = tmplSym.Type
				ft.Name = fd.Sym
			}

			// FIXME: parse an const expr here, but in that case, a ast.SymbolType
			// may contain an ast.Expression (ast.Ast) node which feels weird to me.
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

	util.Printf("parsed ast.RecordType: %v", ret)
	return ret
}

func (self *Parser) parseExternalDecl() ast.Ast {
	defer self.trace("")()
	defer self.handlePanic(lexer.SEMICOLON)

	var tmpl = &ast.Symbol{}
	self.parseTypeDecl(tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(tmpl); decl == nil {
			break
		} else {
			self.tu.Decls = append(self.tu.Decls, decl)
			util.Printf("parsed %v", decl.Repr())
			if _, ok := decl.(*ast.FunctionDecl); ok {
				var fdecl = decl.(*ast.FunctionDecl)

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
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}

done:
	return nil
}

func (self *Parser) parseCompoundStmt() *ast.CompoundStmt {
	defer self.trace("")()
	defer self.handlePanic(lexer.RBRACE)

	var scope = self.PushScope()
	defer func() { self.PopScope() }()
	var compound = &ast.CompoundStmt{Node: self.makeNode(self.peek(0)), Scope: scope}
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

func (self *Parser) parseStatement() ast.Statement {
	defer self.trace("")()
	defer self.handlePanic(lexer.SEMICOLON)

	tok := self.peek(0)

	var stmt ast.Statement
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

		default:
			if ast.IsStorageClass(tok) || ast.IsTypeQualifier(tok) || ast.IsTypeSpecifier(tok) {
				stmt = self.parseDeclStatement()
			}
		}

	}

	if stmt == nil {
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

func (self *Parser) parseIfStatement() *ast.IfStmt {
	defer self.trace("")()

	var ifStmt = &ast.IfStmt{Node: self.makeNode(self.peek(0))}
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
func (self *Parser) parseSwitchStatement() *ast.SwitchStmt {
	defer self.trace("")()

	var switchStmt = &ast.SwitchStmt{Node: self.makeNode(self.peek(0))}
	self.next()
	self.match(lexer.LPAREN)
	switchStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	switchStmt.Body = self.parseStatement()

	return switchStmt
}

func (self *Parser) tolerableParse(rule func() ast.Expression, follow ...lexer.Token) (retVal ast.Expression) {
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

func (self *Parser) parseDoStatement() *ast.DoStmt {
	defer self.trace("")()

	var (
		doStmt = &ast.DoStmt{Node: self.makeNode(self.peek(0))}
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

func (self *Parser) parseWhileStatement() *ast.WhileStmt {
	defer self.trace("")()

	var (
		whileStmt = &ast.WhileStmt{Node: self.makeNode(self.peek(0))}
	)

	self.next()
	self.match(lexer.LPAREN)
	whileStmt.Cond = self.parseExpression(0)
	self.match(lexer.RPAREN)
	whileStmt.Body = self.parseStatement()

	return whileStmt
}

func (self *Parser) parseLabelStatement() *ast.LabelStmt {
	defer self.trace("")()

	var labelStmt = &ast.LabelStmt{Node: self.makeNode(self.peek(0))}

	tok := self.next()
	if tok.Kind != lexer.IDENTIFIER {
		self.parseError(tok, "expect identifier")
	}
	labelStmt.Label = tok.AsString()
	self.match(lexer.COLON)
	labelStmt.Stmt = self.parseStatement()
	return labelStmt
}

func (self *Parser) parseGotoStatement() *ast.GotoStmt {
	defer self.trace("")()

	var gotoStmt = &ast.GotoStmt{Node: self.makeNode(self.peek(0))}

	self.next()
	tok := self.next()
	if tok.Kind != lexer.IDENTIFIER {
		self.parseError(tok, "expect identifier")
	}
	gotoStmt.Label = tok.AsString()
	self.mayIgnore(lexer.SEMICOLON)
	return gotoStmt
}

func (self *Parser) parseContinueStatement() *ast.ContinueStmt {
	defer self.trace("")()

	var continueStmt = &ast.ContinueStmt{Node: self.makeNode(self.peek(0))}

	self.next()
	self.mayIgnore(lexer.SEMICOLON)

	return continueStmt
}

func (self *Parser) parseBreakStatement() *ast.BreakStmt {
	defer self.trace("")()

	var breakStmt = &ast.BreakStmt{Node: self.makeNode(self.peek(0))}

	self.next()
	self.mayIgnore(lexer.SEMICOLON)

	return breakStmt
}

func (self *Parser) parseReturnStatement() *ast.ReturnStmt {
	defer self.trace("")()

	var returnStmt = &ast.ReturnStmt{Node: self.makeNode(self.peek(0))}

	self.next()
	if self.peek(0).Kind != lexer.SEMICOLON {
		returnStmt.Expr = self.parseExpression(0)
	}
	self.mayIgnore(lexer.SEMICOLON)

	return returnStmt
}

// there are two kinds of for ...
func (self *Parser) parseForStatement() *ast.ForStmt {
	defer self.trace("")()

	var (
		forStmt  = &ast.ForStmt{Node: self.makeNode(self.peek(0))}
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
	if ast.IsStorageClass(tok) || ast.IsTypeQualifier(tok) || ast.IsTypeSpecifier(tok) {
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

func (self *Parser) parseCaseStatement() *ast.CaseStmt {
	defer self.trace("")()

	var caseStmt = &ast.CaseStmt{Node: self.makeNode(self.peek(0))}
	self.next()
	caseStmt.ConstExpr = self.parseExpression(0)
	self.match(lexer.COLON)
	caseStmt.Stmt = self.parseStatement()

	return caseStmt
}

func (self *Parser) parseDefaultStatement() *ast.DefaultStmt {
	defer self.trace("")()

	var defaultStmt = &ast.DefaultStmt{Node: self.makeNode(self.peek(0))}
	self.next()
	self.match(lexer.COLON)
	defaultStmt.Stmt = self.parseStatement()

	return defaultStmt
}

func (self *Parser) parseDeclStatement() *ast.DeclStmt {
	defer self.trace("")()

	var declStmt = &ast.DeclStmt{Node: self.makeNode(self.peek(0))}
	var prevParent = self.effectiveParent
	self.effectiveParent = declStmt

	defer func() {
		self.effectiveParent = prevParent
	}()

	var tmpl = &ast.Symbol{}
	self.parseTypeDecl(tmpl)
	for {
		if self.peek(0).Kind == lexer.SEMICOLON {
			self.next()
			break
		}

		if decl := self.parseDeclarator(tmpl); decl == nil {
			break
		} else {
			declStmt.Decls = append(declStmt.Decls, decl)
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}

	return declStmt
}

func (self *Parser) parseExprStatement() (ret *ast.ExprStmt) {
	defer self.trace("")()

	var exprStmt = &ast.ExprStmt{Node: self.makeNode(self.peek(0))}

	exprStmt.Expr = self.tolerableParse(func() ast.Expression {
		return self.parseExpression(0)
	}, lexer.MakeToken(lexer.SEMICOLON, ";"))
	self.mayIgnore(lexer.SEMICOLON)

	if exprStmt.Expr == nil {
		util.Printf(util.Parser, util.Warning, "null expression near %v\n", self.peek(0))
		return nil
	}

	return exprStmt
}

type Associativity int
type Pred int
type Arity int

const (
	NoAssoc Associativity = iota
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
	nud     func(p *Parser, op *operation) ast.Expression
	led     func(p *Parser, lhs ast.Expression, op *operation) ast.Expression
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
func binop_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()

	p.next() // eat op
	var bias = 0
	if op.Associativity == RightAssoc {
		bias = -1
	}

	rhs := p.parseExpression(op.LedPred + bias)

	var expr = &ast.BinaryOperation{p.makeNode(op.Token), op.Token.Kind, lhs, rhs}
	util.Printf("parsed %v", expr.Repr())
	return expr
}

func assign_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()

	p.next() // eat op
	var bias = 0
	if op.Associativity == RightAssoc {
		bias = -1
	}
	rhs := p.parseExpression(op.LedPred + bias)

	var expr = &ast.CompoundAssignExpr{p.makeNode(op.Token), op.Token.Kind, lhs, rhs}
	util.Printf("parsed %v", expr.Repr())
	return expr
}

// ?:
func condop_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()
	var expr = &ast.ConditionalOperation{Node: p.makeNode(op.Token)}

	expr.Cond = lhs

	var bias = 0
	if op.Associativity == RightAssoc {
		bias = -1
	}

	p.next() // eat ?
	expr.True = p.parseExpression(op.LedPred + bias)
	p.match(lexer.COLON) // eat :

	expr.False = p.parseExpression(op.LedPred)

	util.Printf("parsed %v", expr.Repr())
	return expr
}

// for unary sizeof
func sizeof_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	if tok := p.next(); tok.AsString() != "sizeof" {
		p.parseError(tok, "invalid keyword in expression, maybe sizeof ?")
	}

	e := &ast.SizeofExpr{Node: p.makeNode(op.Token)}

	if tok := p.peek(0); tok.Kind == lexer.LPAREN {
		follow := p.peek(1)
		if ast.IsStorageClass(follow) || ast.IsTypeQualifier(follow) || ast.IsTypeSpecifier(follow) {
			p.match(lexer.LPAREN)
			e.Type = p.parseTypeExpression()
			if e.Type == nil {
				p.parseError(p.peek(0), "invalid type name")
			}
			p.match(lexer.RPAREN)
		} else {
			e.Expr = p.parseExpression(op.NudPred)
		}
	} else {
		e.Expr = p.parseExpression(op.NudPred)
	}
	return e
}

// for unary (including prefix)
func unaryop_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	p.next()
	var expr = p.parseExpression(op.NudPred)
	return &ast.UnaryOperation{p.makeNode(op.Token), op.Kind, false, expr}
}

// for postfix
func unaryop_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()
	p.next()

	return &ast.UnaryOperation{p.makeNode(op.Token), op.Kind, true, lhs}
}

// e1.e2  e1->e2
func member_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()
	p.next()

	var expr = &ast.MemberExpr{Node: p.makeNode(op.Token)}
	expr.Target = lhs
	expr.Member = p.parseExpression(op.LedPred)
	if op.Kind == lexer.REFERENCE {
		expr.PointerDeref = true
	}
	return expr
}

// e1[e2]
func array_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()
	p.match(lexer.OPEN_BRACKET)

	var expr = &ast.ArraySubscriptExpr{Node: p.makeNode(op.Token)}
	expr.Target = lhs
	expr.Sub = p.parseExpression(op.LedPred)
	p.match(lexer.CLOSE_BRACKET)
	return expr
}

// could be funcall
func lparen_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	defer p.trace("")()
	p.match(lexer.LPAREN)
	var expr = &ast.FunctionCall{Node: p.makeNode(op.Token)}
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

func (self *Parser) parseTypeExpression() ast.SymbolType {
	var tmpl = &ast.Symbol{}
	self.parseTypeDecl(tmpl)
	if decl := self.parseDeclarator(tmpl); decl != nil {
		if vd, ok := decl.(*ast.VariableDecl); ok {
			sym := self.LookupSymbol(vd.Sym, ast.OrdinaryNS)
			return sym.Type
		}
	}

	return nil
}

func (self *Parser) tryParseTypeExpression() ast.SymbolType {
	defer self.trace("")()
	var ty ast.SymbolType
	tok := self.peek(0)
	if ast.IsStorageClass(tok) || ast.IsTypeQualifier(tok) || ast.IsTypeSpecifier(tok) {
		ty = self.parseTypeExpression()
		if ty == nil {
			self.parseError(tok, "invalid type name for casting")
		}
	}

	return ty
}

// could primary (e), (type){...}, (type)expr
func lparen_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	var (
		cast        *ast.CastExpr
		compoundLit *ast.CompoundLiteralExpr
		ty          ast.SymbolType
		expr        ast.Expression
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
			compoundLit = &ast.CompoundLiteralExpr{Node: p.makeNode(op.Token)}
			compoundLit.Type = ty
			compoundLit.InitList = p.parseInitializerList()
			return compoundLit
		} else {
			cast = &ast.CastExpr{Node: p.makeNode(op.Token)}
			cast.Type = ty
			cast.Expr = p.parseExpression(op.NudPred)
			return cast
		}
	}

	return nil
}

func (self *Parser) parseInitializerList() *ast.InitListExpr {
	defer self.trace("")()
	var (
		compound = false
		expr     ast.Expression
		initList *ast.InitListExpr
	)

	initList = &ast.InitListExpr{Node: self.makeNode(self.peek(0))}

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
			initList.Inits = append(initList.Inits, expr)
			if self.peek(0).Kind == lexer.COMMA {
				self.next()
			}
		}

		self.match(lexer.RBRACE)
	} else {
		expr = self.parseExpression(0)
		initList.Inits = append(initList.Inits, expr)
	}
	operations[lexer.COMMA].LedPred = oldpred

	return initList
}

// for initializer
func brace_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	return p.parseInitializerList()
}

// end of expr
func expr_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	return nil
}

// parse error
func error_led(p *Parser, lhs ast.Expression, op *operation) ast.Expression {
	p.parseError(op.Token, "expect an operator")
	return nil
}

func error_nud(p *Parser, op *operation) ast.Expression {
	p.parseError(op.Token, "expect an expression")
	return nil
}

// for ID
func id_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	p.next()
	return &ast.DeclRefExpr{p.makeNode(op.Token), op.Token.AsString()}
}

// for Literal (int, float, string, char...)
func literal_nud(p *Parser, op *operation) ast.Expression {
	defer p.trace("")()
	p.next()
	switch op.Kind {
	case lexer.INT_LITERAL:
		return &ast.IntLiteralExpr{Node: p.makeNode(op.Token), Tok: op.Token}
	case lexer.STR_LITERAL:
		return &ast.StringLiteralExpr{Node: p.makeNode(op.Token), Tok: op.Token}
	case lexer.CHAR_LITERAL:
		return &ast.CharLiteralExpr{Node: p.makeNode(op.Token), Tok: op.Token}
	}
	return nil
}

func (self *Parser) parseExpression(rbp int) (ret ast.Expression) {
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

func (self *Parser) PushScope() *ast.SymbolScope {
	var scope = &ast.SymbolScope{}
	scope.Parent = self.currentScope
	self.currentScope.Children = append(self.currentScope.Children, scope)

	self.currentScope = scope
	return scope
}

func (self *Parser) PopScope() *ast.SymbolScope {
	if self.currentScope == self.ctx.Top {
		panic("cannot pop top of the scope chain")
	}

	var ret = self.currentScope
	self.currentScope = ret.Parent
	return ret
}

func (self *Parser) AddSymbol(sym *ast.Symbol) {
	var current = self.currentScope
	if sym.NS == ast.TagNS {
	done:
		for ; current != nil; current = current.Parent {
			switch current.Owner.(type) {
			case *ast.CompoundStmt:
				break done
			case *ast.TranslationUnit:
				break done
			}
		}

	}
	current.AddSymbol(sym)
}

func (self *Parser) LookupSymbol(name string, ns ast.SymbolNamespace) *ast.Symbol {
	return self.currentScope.LookupSymbol(name, ns)
}

func (self *Parser) AddNamedType(st ast.SymbolType) {
	var current = self.currentScope

	switch st.(type) {
	case *ast.RecordType, *ast.EnumType:
	done:
		for ; current != nil; current = current.Parent {
			switch current.Owner.(type) {
			case *ast.CompoundStmt:
				break done
			case *ast.TranslationUnit:
				break done
			}
		}
	}

	current.RegisterNamedType(st)
}

func (self *Parser) LookupNamedType(name string, ns ast.SymbolNamespace) ast.SymbolType {
	return self.currentScope.LookupNamedTypeRecursive(name, ns)
}

func (self *Parser) LookupTypedef(name string) ast.SymbolType {
	var current = self.currentScope

	for ; current != nil; current = current.Parent {
		if ty := current.LookupNamedType(name, ast.OrdinaryNS); ty != nil {
			if _, ok := ty.(*ast.UserType); ok {
				return ty
			}
			break
		}
	}
	return nil
}

// this is useless, need to trace symbol hierachy from TU
func (self *Parser) DumpSymbols() {
	var dumpSymbols func(scope *ast.SymbolScope, level int)
	dumpSymbols = func(scope *ast.SymbolScope, level int) {
		for _, sym := range scope.Symbols {
			fmt.Printf("%s%s\n", strings.Repeat(" ", level*2), sym.Name.AsString())
		}

		for _, sub := range scope.Children {
			dumpSymbols(sub, level+1)
		}
	}

	dumpSymbols(self.ctx.Top, 0)
}

func (self *Parser) DumpAst() {
	var (
		stack     int = 0
		arraymode bool
		arraylog  []string
		clr       int
	)

	var walker = struct {
		WalkTranslationUnit      func(ast.WalkStage, *ast.TranslationUnit, *ast.WalkContext)
		WalkIntLiteralExpr       func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) bool
		WalkCharLiteralExpr      func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) bool
		WalkStringLiteralExpr    func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) bool
		WalkBinaryOperation      func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) bool
		WalkDeclRefExpr          func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool
		WalkUnaryOperation       func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) bool
		WalkSizeofExpr           func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) bool
		WalkConditionalOperation func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) bool
		WalkArraySubscriptExpr   func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) bool
		WalkMemberExpr           func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool
		WalkFunctionCall         func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool
		WalkCompoundAssignExpr   func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) bool
		WalkCastExpr             func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) bool
		WalkImplicitCastExpr     func(ws ast.WalkStage, e *ast.ImplicitCastExpr, ctx *ast.WalkContext) bool
		WalkCompoundLiteralExpr  func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) bool
		WalkInitListExpr         func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) bool
		WalkFieldDecl            func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext)
		WalkRecordDecl           func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext)
		WalkEnumeratorDecl       func(ws ast.WalkStage, e *ast.EnumeratorDecl, ctx *ast.WalkContext)
		WalkEnumDecl             func(ws ast.WalkStage, e *ast.EnumDecl, ctx *ast.WalkContext)
		WalkVariableDecl         func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext)
		WalkTypedefDecl          func(ws ast.WalkStage, e *ast.TypedefDecl, ctx *ast.WalkContext)
		WalkParamDecl            func(ws ast.WalkStage, e *ast.ParamDecl, ctx *ast.WalkContext)
		WalkFunctionDecl         func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext)
		WalkExprStmt             func(ws ast.WalkStage, e *ast.ExprStmt, ctx *ast.WalkContext)
		WalkLabelStmt            func(ws ast.WalkStage, e *ast.LabelStmt, ctx *ast.WalkContext)
		WalkCaseStmt             func(ws ast.WalkStage, e *ast.CaseStmt, ctx *ast.WalkContext)
		WalkDefaultStmt          func(ws ast.WalkStage, e *ast.DefaultStmt, ctx *ast.WalkContext)
		WalkReturnStmt           func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext)
		WalkIfStmt               func(ws ast.WalkStage, e *ast.IfStmt, ctx *ast.WalkContext)
		WalkSwitchStmt           func(ws ast.WalkStage, e *ast.SwitchStmt, ctx *ast.WalkContext)
		WalkWhileStmt            func(ws ast.WalkStage, e *ast.WhileStmt, ctx *ast.WalkContext)
		WalkDoStmt               func(ws ast.WalkStage, e *ast.DoStmt, ctx *ast.WalkContext)
		WalkDeclStmt             func(ws ast.WalkStage, e *ast.DeclStmt, ctx *ast.WalkContext)
		WalkForStmt              func(ws ast.WalkStage, e *ast.ForStmt, ctx *ast.WalkContext)
		WalkGotoStmt             func(ws ast.WalkStage, e *ast.GotoStmt, ctx *ast.WalkContext)
		WalkContinueStmt         func(ws ast.WalkStage, e *ast.ContinueStmt, ctx *ast.WalkContext)
		WalkBreakStmt            func(ws ast.WalkStage, e *ast.BreakStmt, ctx *ast.WalkContext)
		WalkCompoundStmt         func(ws ast.WalkStage, e *ast.CompoundStmt, ctx *ast.WalkContext)
	}{}

	var log = func(msg string) {
		if arraymode {
			arraylog = append(arraylog, msg)
		} else {
			if 1 == stack {
				clr = rand.Intn(200) + 50
			}
			fmt.Print(fmt.Sprintf("\033[38;5;%dm%s%s\033[00m\n", clr, strings.Repeat("  ", stack), msg))
		}
	}

	walker.WalkTranslationUnit = func(ws ast.WalkStage, tu *ast.TranslationUnit, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("TranslationUnit")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkIntLiteralExpr = func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, e.Tok.AsString())
				return false
			} else {
				log(e.Repr())
			}
		}
		return true
	}

	walker.WalkCharLiteralExpr = func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, e.Tok.AsString())
				return false
			} else {
				log(e.Repr())
			}
		}
		return true
	}
	walker.WalkStringLiteralExpr = func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, e.Tok.AsString())
				return false
			} else {
				log(e.Repr())
			}
		}
		return true
	}

	walker.WalkBinaryOperation = func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				ast.WalkAst(e.LHS, walker)
				arraylog = append(arraylog, lexer.TokKinds[e.Op])
				ast.WalkAst(e.RHS, walker)
				return false

			} else {
				var ty = reflect.TypeOf(e).Elem()
				var inferedType = ""
				if e.GetType() != nil {
					inferedType = e.GetType().String()
				}
				log(fmt.Sprintf("%s(%s '%s')", ty.Name(), inferedType, lexer.TokKinds[e.Op]))
			}
			stack++
		} else {
			stack--
		}
		return true
	}

	walker.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, e.Name)
				return false
			} else {
				log(e.Repr())
			}
		}
		return true
	}

	walker.WalkSizeofExpr = func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, "sizeof ")
				if e.Type != nil {
					arraylog = append(arraylog, fmt.Sprintf("(%s)", e.Type))
				} else {
					ast.WalkAst(e.Expr, walker)
				}
				return false
			}

			if e.Type == nil {
				log("SizeofExpr")
			} else {
				log(fmt.Sprintf("ast.SizeofExpr(%v)", e.Type))
			}
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkUnaryOperation = func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				if e.Postfix {
					ast.WalkAst(e.Expr, walker)
					arraylog = append(arraylog, lexer.TokKinds[e.Op])
				} else {
					arraylog = append(arraylog, "(")
					arraylog = append(arraylog, lexer.TokKinds[e.Op])
					ast.WalkAst(e.Expr, walker)
					arraylog = append(arraylog, ")")
				}
				return false

			} else {
				var ty = reflect.TypeOf(e).Elem()
				var inferedType = ""
				if e.GetType() != nil {
					inferedType = e.GetType().String()
				}
				if e.Postfix {
					log(fmt.Sprintf("%s(postfix %s '%s')", ty.Name(), inferedType, lexer.TokKinds[e.Op]))
				} else {
					log(fmt.Sprintf("%s(prefix %s '%s')", ty.Name(), inferedType, lexer.TokKinds[e.Op]))
				}
			}
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkConditionalOperation = func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				ast.WalkAst(e.Cond, walker)
				arraylog = append(arraylog, "?")
				ast.WalkAst(e.True, walker)
				arraylog = append(arraylog, ":")
				ast.WalkAst(e.False, walker)
				return false
			}
			log("ConditionalOperation")
			stack++
		} else {
			stack--
		}
		return true
	}

	walker.WalkArraySubscriptExpr = func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if !arraymode {
				log("ArraySubscriptExpr")
			} else {
				ast.WalkAst(e.Target, walker)
				arraylog = append(arraylog, "[")
				ast.WalkAst(e.Sub, walker)
				arraylog = append(arraylog, "]")
				return false
			}
			stack++
		} else {
			stack--
		}
		return true
	}

	walker.WalkMemberExpr = func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				ast.WalkAst(e.Target, walker)
				arraylog = append(arraylog, ".")
				ast.WalkAst(e.Member, walker)
				return false
			}

			log("MemberExpr")
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				ast.WalkAst(e.Func, walker)
				arraylog = append(arraylog, "(")
				for _, a := range e.Args {
					ast.WalkAst(a, walker)
				}
				arraylog = append(arraylog, ")")

				return false
			}
			log("FunctionCall")
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkCompoundAssignExpr = func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				ast.WalkAst(e.LHS, walker)
				arraylog = append(arraylog, lexer.TokKinds[e.Op])
				ast.WalkAst(e.RHS, walker)
				return false
			}
			var ty = reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%s)", ty.Name(), lexer.TokKinds[e.Op]))
			stack++
		} else {
			stack--
		}
		return true
	}

	walker.WalkCastExpr = func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, fmt.Sprintf("(%s)", e.Type))
				ast.WalkAst(e.Expr, walker)
				return false
			}
			log(fmt.Sprintf("CastExpr(%s)", e.Type))
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkImplicitCastExpr = func(ws ast.WalkStage, e *ast.ImplicitCastExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, fmt.Sprintf("<%s>", e.DestType))
				ast.WalkAst(e.Expr, walker)
				return false
			}
			log(fmt.Sprintf("ImplicitCastExpr(%s <%s>)", e.DestType, e.CastKind))
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkCompoundLiteralExpr = func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, fmt.Sprintf("(%s)", e.Type))
				ast.WalkAst(e.InitList, walker)
				return false
			}
			log(fmt.Sprintf("CompoundLiteralExpr(%s)", e.Type))
			stack++
		} else {
			stack--
		}
		return true
	}

	walker.WalkBreakStmt = func(ws ast.WalkStage, e *ast.BreakStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("BreakStmt")
		}
	}

	walker.WalkContinueStmt = func(ws ast.WalkStage, e *ast.ContinueStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("ContinueStmt")
		}
	}

	walker.WalkInitListExpr = func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if arraymode {
				arraylog = append(arraylog, "{")
				for _, e2 := range e.Inits {
					ast.WalkAst(e2, walker)
				}
				arraylog = append(arraylog, "}")
				return false
			}
			log("InitListExpr")
			stack++
		} else {
			stack--
		}
		return true
	}
	walker.WalkFieldDecl = func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log(fmt.Sprintf("FieldDecl(%s)", e.Sym))
		}
	}
	walker.WalkRecordDecl = func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.TagNS)

			ty := "struct"
			if sym.Type.(*ast.RecordType).Union {
				ty = "union"
			}

			if e.Prev != nil {
				log(fmt.Sprintf("RecordDecl(%s %s prev %p)", ty, e.Sym, e.Prev))
			} else {
				log(fmt.Sprintf("RecordDecl(%s %s)", ty, e.Sym))
			}
			stack++

		} else {
			stack--
		}
	}
	walker.WalkEnumeratorDecl = func(ws ast.WalkStage, e *ast.EnumeratorDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log(fmt.Sprintf("Enumerator(%s)", e.Sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkEnumDecl = func(ws ast.WalkStage, e *ast.EnumDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log(fmt.Sprintf("EnumDecl(%s prev %p)", e.Sym, e.Prev))
			stack++

		} else {
			stack--
		}
	}
	walker.WalkTypedefDecl = func(ws ast.WalkStage, e *ast.TypedefDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)

			log(fmt.Sprintf("TypedefDecl(%s)", sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkVariableDecl = func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext) {
		sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
		if ws == ast.WalkerPropagate {
			if ty, isArray := sym.Type.(*ast.Array); isArray {
				arraymode = true
				for _, expr := range ty.LenExprs {
					arraylog = append(arraylog, "[")
					ast.WalkAst(expr, walker)
					arraylog = append(arraylog, "]")
				}
				arraymode = false
				log(fmt.Sprintf("VarDecl('%s' %s)", strings.Join(arraylog, ""), e.Sym))
				arraylog = nil
			} else {
				log(fmt.Sprintf("VarDecl(%s)", sym))
			}

			stack++
		} else {
			stack--
		}
	}
	walker.WalkParamDecl = func(ws ast.WalkStage, e *ast.ParamDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
			ty := reflect.TypeOf(e).Elem()
			log(fmt.Sprintf("%s(%v)", ty.Name(), sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkFunctionDecl = func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Name, ast.OrdinaryNS)
			log(fmt.Sprintf("FuncDecl(%v)", sym))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkExprStmt = func(ws ast.WalkStage, e *ast.ExprStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("ExprStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkLabelStmt = func(ws ast.WalkStage, e *ast.LabelStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log(fmt.Sprintf("LabelStmt(%s)", e.Label))
			stack++
		} else {
			stack--
		}
	}

	walker.WalkCaseStmt = func(ws ast.WalkStage, e *ast.CaseStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("CaseStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDefaultStmt = func(ws ast.WalkStage, e *ast.DefaultStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("DefaultStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkReturnStmt = func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("ReturnStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkSwitchStmt = func(ws ast.WalkStage, e *ast.SwitchStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("SwitchStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkWhileStmt = func(ws ast.WalkStage, e *ast.WhileStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("WhileStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDoStmt = func(ws ast.WalkStage, e *ast.DoStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("DoStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkDeclStmt = func(ws ast.WalkStage, e *ast.DeclStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("DeclStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkIfStmt = func(ws ast.WalkStage, e *ast.IfStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("IfStmt")
			stack++
		} else {
			stack--
		}
	}

	walker.WalkGotoStmt = func(ws ast.WalkStage, e *ast.GotoStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log(fmt.Sprintf("Goto(%s)", e.Label))
			stack++
		} else {
			stack--
		}
	}
	walker.WalkForStmt = func(ws ast.WalkStage, e *ast.ForStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {

			log("ForStmt")
			stack++
		} else {
			stack--
		}
	}
	walker.WalkCompoundStmt = func(ws ast.WalkStage, e *ast.CompoundStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			log("CompoundStmt")
			stack++
		} else {
			stack--
		}
	}

	ast.WalkAst(self.tu, walker)
}

func (self *Parser) handlePanic(kd lexer.Kind) {
	defer self.trace("")()
	if p := recover(); p != nil {

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
	operations[lexer.AND] = &operation{lexer.Token{}, LeftAssoc, 140, 80, unaryop_nud, binop_led}

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

	// for sizeof unary op
	operations[lexer.KEYWORD] = &operation{lexer.Token{}, LeftAssoc, 140, -1, sizeof_nud, error_led}

	operations[lexer.OPEN_BRACKET] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, array_led}
	operations[lexer.CLOSE_BRACKET] = &operation{lexer.Token{}, LeftAssoc, -1, -1, error_nud, expr_led}
	operations[lexer.DOT] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, member_led}
	operations[lexer.REFERENCE] = &operation{lexer.Token{}, LeftAssoc, -1, 160, error_nud, member_led}

	operations[lexer.INT_LITERAL] = &operation{lexer.Token{}, NoAssoc, 200, -1, literal_nud, error_led}
	operations[lexer.STR_LITERAL] = &operation{lexer.Token{}, NoAssoc, 200, -1, literal_nud, error_led}
	operations[lexer.CHAR_LITERAL] = &operation{lexer.Token{}, NoAssoc, 200, -1, literal_nud, error_led}
	operations[lexer.IDENTIFIER] = &operation{lexer.Token{}, NoAssoc, 200, -1, id_nud, error_led}

	// this is for cast-expr, compoundinitexpr
	operations[lexer.LBRACE] = &operation{lexer.Token{}, NoAssoc, 150, -1, brace_nud, expr_led}
	operations[lexer.RBRACE] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, expr_led}

	operations[lexer.SEMICOLON] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, expr_led}

	operations[lexer.ERROR] = &operation{lexer.Token{}, NoAssoc, -1, -1, error_nud, error_led}
}
