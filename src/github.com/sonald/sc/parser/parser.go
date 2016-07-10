package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"log"
	"os"
	_ "reflect"
)

const NR_LA = 4

type Parser struct {
	lex    *lexer.Scanner
	tokens [NR_LA]lexer.Token // support 4-lookahead
	cursor int
	eot    bool // meat EOT

	top   *SymbolScope
	types []SymbolType
	tu    *TranslationUnit
}

type ParseOption struct {
	filename string
}

func NewParser() *Parser {
	p := &Parser{}

	return p
}

func (self *Parser) peek(n int) lexer.Token {
	if n >= NR_LA || n < 0 {
		panic(fmt.Sprintf("only maximum of %d tokens lookahead supported", NR_LA))
	}

	tok := self.tokens[n]
	log.Printf("peek %s(%s)\n", lexer.TokKinds[tok.Kind], tok.AsString())
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
	//log.Printf("next %s(%s)\n", lexer.TokKinds[tok.Kind], tok.AsString())
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
		log.Printf("%s\n", err.Error())
		return nil
	} else {
		self.lex = lexer.NewScanner(f)
	}

	for i := range self.tokens {
		self.tokens[i] = self.getNextToken()
	}

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
	panic(fmt.Sprintf("tok %v, %s", tok, msg))
}

/*

external-declaration: function-definition | declaration
function-definition:
	declaration-specifiers declarator declaration-list? compound-statement
declaration-list: declaration+

declaration:
	declaration-specifiers init-declarator-list? ;

declaration-specifiers:
	storage-class-specifier declaration-specifiers?
	type-specifier declaration-specifiers?
	type-qualifier declaration-specifiers?
	function-specifier declaration-specifiers?

init-declarator-list: init-declarator
	init-declarator-list , init-declarator

init-declarator: declarator
	declarator = initializer
*/

func (self *Parser) parseTypeDecl(opts *ParseOption, sym *Symbol) {
	log.Println("parseTypeDecl")
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

	log.Printf("parsed type template %v", sym)
}

func (self *Parser) parseFunctionParams(opts *ParseOption, decl *FunctionDecl) {
	log.Println("parseFunctionParams")
	var ty = decl.Name.Type.(*Function)
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
			case *VariableDeclaration:
				var pd = &ParamDecl{arg.(*VariableDeclaration).Sym}
				decl.Args = append(decl.Args, pd)
				ty.Args = append(ty.Args, pd.Sym.Type)
				log.Printf("parsed arg %v", pd.Repr())
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
	log.Println("parseDeclarator")
	var newSym = Symbol{Type: sym.Type, Storage: sym.Storage}

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
		if tok := self.next(); tok.Kind == lexer.INT_LITERAL {
			newSym.Type = &Array{newSym.Type, 1, []int{tok.AsInt()}}
		} else {
			self.parseError(tok, "invalid array type specifier")
		}
		self.match(lexer.CLOSE_BRACKET)

		decl = &VariableDeclaration{Sym: &newSym}

	case lexer.LPAREN: // func
		self.match(lexer.LPAREN)
		var fdecl = &FunctionDecl{}
		decl = fdecl
		newSym.Type = &Function{Return: newSym.Type}
		fdecl.Name = &newSym
		self.parseFunctionParams(opts, fdecl)
		self.match(lexer.RPAREN)

	default:
		decl = &VariableDeclaration{Sym: &newSym}

		if self.peek(0).Kind == lexer.EQUAL {
			// parse initializer
			//decl.(&VariableDeclaration).init = init
		}
	}

	return decl
}

func (self *Parser) parseExternalDecl(opts *ParseOption) Ast {
	log.Println("parseExternalDecl")

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
			case *VariableDeclaration:
				self.tu.varDecls = append(self.tu.varDecls, decl)
				log.Printf("parsed %v", decl.Repr())
			case *FunctionDecl:
				self.tu.funcDecls = append(self.tu.funcDecls, decl)
				log.Printf("parsed %v", decl.Repr())
			default:
				self.parseError(self.peek(0), "")
			}
		}

		if self.peek(0).Kind == lexer.COMMA {
			self.next()
		}
	}
	return nil
}

func init() {
	log.Println("init parser")

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
}
