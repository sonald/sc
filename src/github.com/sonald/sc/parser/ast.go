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
	ty := reflect.TypeOf(tu).Elem()
	return fmt.Sprintf("%s(%s)", ty.Name(), tu.filename)
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

type CharLiteralExpr struct {
	Node
	Tok lexer.Token
}

func (self *CharLiteralExpr) Repr() string {
	return fmt.Sprintf("CharLit(%v)", self.Tok.AsString())
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
	Op       lexer.Kind
	LHS, RHS Expression
}

func (self *BinaryOperation) Repr() string {
	var (
		ty  = reflect.TypeOf(self).Elem()
		ty2 = reflect.TypeOf(self.LHS).Elem()
		ty3 = reflect.TypeOf(self.RHS).Elem()
	)
	return fmt.Sprintf("%s(Op(%s) %v %v)", ty.Name(), lexer.TokKinds[self.Op],
		ty2.Name(), ty3.Name())
}

type DeclRefExpr struct {
	Node
	Name string // symbol name for declaration
}

func (self *DeclRefExpr) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	return fmt.Sprintf("%s(%s)", ty.Name(), self.Name)
}

type UnaryOperation struct {
	Node
	Op      lexer.Kind
	Postfix bool // false if prefix, true postfix. eg ++, --
	expr    Expression
}

func (self *UnaryOperation) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	ts := lexer.TokKinds[self.Op]
	if self.Postfix {
		return fmt.Sprintf("%s(Postfix(%s) %s)", ty.Name(), self.expr, ts)
	} else {
		return fmt.Sprintf("%s(%s %s)", ty.Name(), ts, self.expr)
	}
}

type ConditionalOperation struct {
	Node
	Cond  Expression
	True  Expression
	False Expression
}

func (self *ConditionalOperation) Repr() string {
	var (
		ty  = reflect.TypeOf(self).Elem()
		ty2 = reflect.TypeOf(self.Cond).Elem()
		ty3 = reflect.TypeOf(self.True).Elem()
		ty4 = reflect.TypeOf(self.False).Elem()
	)
	return fmt.Sprintf("%s(Cond(%s) %v %v)", ty.Name(), ty2.Name(), ty3.Name(), ty4.Name())
}

//--------------------------------------------------------------------------------

// considered abstract
type Statement interface {
	Ast
}

type ExprStmt struct {
	Node
	Expr Expression
}

func (self *ExprStmt) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	return fmt.Sprintf("%s(%s)", ty.Name(), self.Expr.Repr())
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
	return fmt.Sprintf("DeclStmt(%d vars)", len(self.Decls))
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
