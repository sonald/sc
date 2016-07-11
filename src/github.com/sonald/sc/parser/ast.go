// Package ast provides AST
package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"reflect"
)

type Ast interface {
	Repr() string
}

type AstContext struct {
	top          *SymbolScope
	currentScope *SymbolScope
	types        []SymbolType
}

type Node struct {
	ctx *AstContext
}

type TranslationUnit struct {
	Node
	filename  string
	funcDecls []*FunctionDecl
	varDecls  []*VariableDecl
}

func (tu *TranslationUnit) Repr() string {
	return fmt.Sprintf("%v", *tu)
}

// considered abstract
type Expression interface {
	Ast
}

type IntLiteralExpr struct {
	Node
	Tok lexer.Token
}

func (self *IntLiteralExpr) Repr() string {
	return fmt.Sprintf("IntLit(%v)", self.Tok.AsString())
}

type StringLiteralExpr struct {
	Node
	Tok lexer.Token
}

func (self *StringLiteralExpr) Repr() string {
	return fmt.Sprintf("StrLit(%v)", self.Tok.AsString())
}

type BinaryOperation struct {
	Node
	Op       lexer.Token
	LHS, RHS *Expression
}

func (self *BinaryOperation) Repr() string {
	return fmt.Sprintf("BinOp(Op(%s) %v %v)", self.Op.AsString(),
		self.LHS, self.RHS)
}

// considered abstract
type Statement interface {
	Ast
}

type ExpressionStmt struct {
	Node
	Expr Expression
}

func (self *ExpressionStmt) Repr() string {
	return fmt.Sprintf("ExprStmt(%v)", self.Expr)
}

type VariableDecl struct {
	Node
	Sym  string
	init Expression
}

func (self *VariableDecl) Repr() string {
	return fmt.Sprintf("VarDecl(%s)", self.Sym)
}

type Initializer struct {
	Node
}

func (self *Initializer) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type ParamDecl struct {
	Node
	Sym string
}

func (self *ParamDecl) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	return fmt.Sprintf("%s(%v)", ty.Name(), self.Sym)
}

type FunctionDecl struct {
	Node
	Name  string
	Args  []*ParamDecl
	Body  *CompoundStmt
	Scope *SymbolScope
}

func (self *FunctionDecl) Repr() string {
	sym := self.Scope.LookupSymbol(self.Name)
	var ty = sym.Type.(*Function)
	var stg = ""
	if sym.Storage != NilStorage {
		stg = sym.Storage.String() + " "
	}
	var s = fmt.Sprintf("FuncDecl(%v%v %v", stg, ty.Return, self.Name)

	s += "("
	for i, arg := range self.Args {
		s += arg.Repr()
		if i < len(self.Args)-1 {
			s += ", "
		}
	}
	s += "))"

	if self.Body != nil {
		s += self.Body.Repr()
	}
	return s
}

type LabelStmt struct {
	Node
	Label string
	Stmt  Statement
}

func (self *LabelStmt) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	return fmt.Sprintf("%s(%v)", ty.Name(), *self)
}

type CaseStmt struct {
	Node
	ConstExpr Expression
	Stmt      Statement
}

func (self *CaseStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type DefaultStmt struct {
	Node
	Stmt Statement
}

func (self *DefaultStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type ReturnStmt struct {
	Node
	Stmt Statement
}

func (self *ReturnStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type IfStmt struct {
	Node
	Cond        Expression
	TrueBranch  Statement
	FalseBranch Statement
}

func (self *IfStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type SwitchStmt struct {
	Node
	Cond Expression
	Body Statement
}

func (self *SwitchStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type WhileStmt struct {
	Node
	Cond Expression
	Body Statement
}

func (self *WhileStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type DoStmt struct {
	Node
	Cond Expression
	Body Statement
}

func (self *DoStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type DeclStmt struct {
	Node
	Decls []*VariableDecl
}

func (self *DeclStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

// Decl/Init can not coexists, either one is assigned, the other should be nil
type ForStmt struct {
	Node
	Decl *DeclStmt
	Init Expression

	Cond Expression
	Step Expression
	Body Statement
}

func (self *ForStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type GotoStmt struct {
	Node
	Label string
}

func (self *GotoStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type ContinueStmt struct {
	Node
}

func (self *ContinueStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type BreakStmt struct {
	Node
}

func (self *BreakStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}

type CompoundStmt struct {
	Node
	Stmts []Statement
	Scope *SymbolScope
}

func (self *CompoundStmt) Repr() string {
	return fmt.Sprintf("%v", *self)
}
