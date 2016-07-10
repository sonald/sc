// Package ast provides AST
package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	_ "reflect"
)

type Ast interface {
	Repr() string
}

type TranslationUnit struct {
	filename  string
	funcDecls []Ast
	varDecls  []Ast
}

func (tu *TranslationUnit) Repr() string {
	return fmt.Sprintf("%v", *tu)
}

// considered abstract
type Expression interface {
	Ast
}

type IntLiteralExpr struct {
	tok lexer.Token
}

func (self *IntLiteralExpr) Repr() string {
	return fmt.Sprintf("%v", self.tok.AsString())
}

type StringLiteralExpr struct {
	tok lexer.Token
}

func (self *StringLiteralExpr) Repr() string {
	return fmt.Sprintf("%v", self.tok.AsString())
}

type BinaryOperation struct {
	op       lexer.Token
	lhs, rhs *Expression
}

func (self *BinaryOperation) Repr() string {
	return fmt.Sprintf("%v", *self)
}

// considered abstract
type Statement interface {
	Ast
}

type ExpressionStatement struct {
	expr Expression
}

func (self *ExpressionStatement) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type VariableDeclaration struct {
	Sym  *Symbol
	init Expression
}

func (self *VariableDeclaration) Repr() string {
	return fmt.Sprintf("VarDecl(%s)", self.Sym)
}

type Initializer struct {
}

func (self *Initializer) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type ParamDecl struct {
	Sym *Symbol
}

func (self *ParamDecl) Repr() string {
	return fmt.Sprintf("ParamDecl(%v)", self.Sym)
}

type FunctionDecl struct {
	Name *Symbol
	Args []*ParamDecl
	body Statement // a *BlockStatement: may be nil if it's external decl
}

func (self *FunctionDecl) Repr() string {
	var ty = self.Name.Type.(*Function)
	var stg = ""
	if self.Name.Storage != NilStorage {
		stg = self.Name.Storage.String() + " "
	}
	var s = fmt.Sprintf("%v%v %v", stg, ty.Return, self.Name.Name.AsString())

	s += "("
	for i, arg := range self.Args {
		s += arg.Repr()
		if i < len(self.Args)-1 {
			s += ", "
		}
	}
	s += ")"

	if self.body != nil {
		s += self.body.Repr()
	}
	return s
}

type BlockStatement struct {
	Stmts []Statement
}

func (self *BlockStatement) Repr() string {
	return fmt.Sprintf("%v", *self)
}
