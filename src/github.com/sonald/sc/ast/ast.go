// Package ast provides AST
package ast

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"reflect"
)

type Ast interface {
	Repr() string
}

type AstContext struct {
	Top *SymbolScope
}

type Node struct {
	Ctx *AstContext
}

func (n *Node) Repr() string {
	return "#"
}

type TranslationUnit struct {
	Node
	Filename     string
	FuncDecls    []*FunctionDecl
	VarDecls     []*VariableDecl
	RecordDecls  []*RecordDecl
	EnumDecls    []*EnumDecl
	TypedefDecls []*TypedefDecl
}

func (tu *TranslationUnit) Repr() string {
	ty := reflect.TypeOf(tu).Elem()
	return fmt.Sprintf("%s(%s)", ty.Name(), tu.Filename)
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
	Expr    Expression
}

func (self *UnaryOperation) Repr() string {
	ty := reflect.TypeOf(self).Elem()
	ts := lexer.TokKinds[self.Op]
	if self.Postfix {
		return fmt.Sprintf("%s(Postfix(%s) %s)", ty.Name(), self.Expr, ts)
	} else {
		return fmt.Sprintf("%s(%s %s)", ty.Name(), ts, self.Expr)
	}
}

type SizeofExpr struct {
	Node
	// type is either from source code or evaluated from Expr
	Type SymbolType
	Expr Expression
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

// Target[Sub]
type ArraySubscriptExpr struct {
	Node
	Target Expression
	Sub    Expression
}

// for . and ->
type MemberExpr struct {
	Node
	Target Expression
	Member Expression
}

type FunctionCall struct {
	Node
	Func Expression
	Args []Expression
}

// inspired from llvm, to distinguished from BinaryOp
type CompoundAssignExpr struct {
	Node
	Op  lexer.Kind
	LHS Expression
	RHS Expression
}

type CastExpr struct {
	Node
	Type SymbolType
	Expr Expression
}

type CompoundLiteralExpr struct {
	Node
	Type     SymbolType
	InitList *InitListExpr
}

type InitListExpr struct {
	Node
	Inits []Expression
}

//--------------------------------------------------------------------------------

// considered abstract
type Statement interface {
	Ast
}

type FieldDecl struct {
	Node
	Sym string // Name of Field's Symbol
	Loc lexer.Location
}

func (self *FieldDecl) Repr() string {
	return fmt.Sprintf("FieldDecl(%s)", self.Sym)
}

type RecordDecl struct {
	Node
	// Name of Record's Symbol, this symbol's type is identical to
	// this record variable's type
	Sym          string
	Fields       []*FieldDecl
	Loc          lexer.Location
	Scope        *SymbolScope
	IsDefinition bool
	Prev         Ast // previous forward declaration of the record
}

func (self *RecordDecl) Repr() string {
	return fmt.Sprintf("RecordDecl(%s)", self.Sym)
}

type EnumeratorDecl struct {
	Node
	Sym   string     // Name of Enumerator's Symbol
	Value Expression // need to be a constant-expr
	Loc   lexer.Location
}

func (self *EnumeratorDecl) Repr() string {
	return fmt.Sprintf("EnumeratorDecl(%s)", self.Sym)
}

type EnumDecl struct {
	Node
	Sym          string
	List         []*EnumeratorDecl
	Loc          lexer.Location
	IsDefinition bool
	Prev         Ast // previous forward declaration of the enum
}

func (self *EnumDecl) Repr() string {
	return fmt.Sprintf("EnumDecl(%s)", self.Sym)
}

type TypedefDecl struct {
	Node
	Sym string
	Loc lexer.Location
}

func (self *TypedefDecl) Repr() string {
	return fmt.Sprintf("Typedef(%s)", self.Sym)
}

type VariableDecl struct {
	Node
	Sym  string
	Init Expression
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
	Name       string
	Args       []*ParamDecl
	IsVariadic bool
	Body       *CompoundStmt
	Scope      *SymbolScope
}

func (self *FunctionDecl) Repr() string {
	sym := self.Scope.LookupSymbol(self.Name, false)
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
	if self.IsVariadic {
		s += ",..."
	}
	s += "))"

	if self.Body != nil {
		s += self.Body.Repr()
	}
	return s
}

//FIXME: should Label be a symbol
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

type ReturnStmt struct {
	Node
	Expr Expression
}

type IfStmt struct {
	Node
	Cond        Expression
	TrueBranch  Statement
	FalseBranch Statement
}

// CaseStmt usually resides in CompoundStmt
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

type DoStmt struct {
	Node
	Cond Expression
	Body Statement
}

type DeclStmt struct {
	Node
	Decls        []*VariableDecl
	RecordDecls  []*RecordDecl
	EnumDecls    []*EnumDecl
	TypedefDecls []*TypedefDecl
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
	// if Decl exists, this points to a subscope, else it equals to parent scope
	Scope *SymbolScope
}

type GotoStmt struct {
	Node
	Label string
}

type ContinueStmt struct {
	Node
}

type BreakStmt struct {
	Node
}

type CompoundStmt struct {
	Node
	Stmts []Statement
	Scope *SymbolScope
}

type ExprStmt struct {
	Node
	Expr Expression
}

// walk stage
type WalkStage int

const (
	WalkerPropagate WalkStage = iota
	WalkerBubbleUp
)

// AstWalker is empty and defined nothing at all
// It requires that any object that implement AstWalker should contains
// some functions like Wak*AstType*(WalkStage, Ast), e.g
//		WalkTranslationUnit(WalkStage, ast Ast) bool
// or without return arg
//		WalkArraySubscriptExpr(WalkStage, ast Ast)
// the return value bool is optional, if defined, return false to stop
// traversal.
type AstWalker interface {
}

func WalkAst(top Ast, wk AstWalker) {
	var wkValue = reflect.ValueOf(wk)
	var visit func(ast Ast)

	var tryCall = func(stage WalkStage, ast Ast) bool {
		var method = wkValue.FieldByName("Walk" + reflect.TypeOf(ast).Elem().Name())
		if method.IsValid() {
			ret := method.Call([]reflect.Value{reflect.ValueOf(stage), reflect.ValueOf(ast)})
			if len(ret) > 0 && ret[0].Kind() == reflect.Bool && !ret[0].Bool() {
				return false
			}
		}
		return true
	}

	visit = func(ast Ast) {
		switch ast.(type) {
		case *TranslationUnit:
			tu := ast.(*TranslationUnit)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, d := range tu.VarDecls {
				visit(d)
			}

			for _, d := range tu.RecordDecls {
				visit(d)
			}

			for _, d := range tu.EnumDecls {
				visit(d)
			}

			for _, d := range tu.TypedefDecls {
				visit(d)
			}

			for _, d := range tu.FuncDecls {
				visit(d)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *IntLiteralExpr:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CharLiteralExpr:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *StringLiteralExpr:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *BinaryOperation:
			var e = ast.(*BinaryOperation)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.LHS)
			visit(e.RHS)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *DeclRefExpr:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *UnaryOperation:
			var e = ast.(*UnaryOperation)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Expr)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *SizeofExpr:
			var e = ast.(*SizeofExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if e.Expr != nil {
				visit(e.Expr)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ConditionalOperation:
			e := ast.(*ConditionalOperation)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Cond)
			visit(e.True)
			visit(e.False)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ArraySubscriptExpr:
			e := ast.(*ArraySubscriptExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Target)
			visit(e.Sub)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *MemberExpr:
			e := ast.(*MemberExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Target)
			visit(e.Member)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *FunctionCall:
			e := ast.(*FunctionCall)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Func)
			for _, arg := range e.Args {
				visit(arg)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CompoundAssignExpr:
			var e = ast.(*CompoundAssignExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.LHS)
			visit(e.RHS)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CastExpr:
			e := ast.(*CastExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Expr)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CompoundLiteralExpr:
			e := ast.(*CompoundLiteralExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.InitList)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *InitListExpr:
			e := ast.(*InitListExpr)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, init := range e.Inits {
				visit(init)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *FieldDecl:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *RecordDecl:
			e := ast.(*RecordDecl)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, f := range e.Fields {
				visit(f)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *EnumeratorDecl:
			e := ast.(*EnumeratorDecl)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if e.Value != nil {
				visit(e.Value)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *EnumDecl:
			e := ast.(*EnumDecl)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, f := range e.List {
				visit(f)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *TypedefDecl:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *VariableDecl:
			e := ast.(*VariableDecl)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if e.Init != nil {
				visit(e.Init)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ParamDecl:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *FunctionDecl:
			e := ast.(*FunctionDecl)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, arg := range e.Args {
				visit(arg)
			}

			if e.Body != nil {
				visit(e.Body)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ExprStmt:
			e := ast.(*ExprStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Expr)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *LabelStmt:
			e := ast.(*LabelStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Stmt)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CaseStmt:
			e := ast.(*CaseStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.ConstExpr)
			visit(e.Stmt)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *DefaultStmt:
			e := ast.(*DefaultStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Stmt)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ReturnStmt:
			e := ast.(*ReturnStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Expr)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *IfStmt:
			e := ast.(*IfStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Cond)
			if e.TrueBranch != nil {
				visit(e.TrueBranch)
			}
			if e.FalseBranch != nil {
				visit(e.FalseBranch)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *SwitchStmt:
			e := ast.(*SwitchStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Cond)
			visit(e.Body)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *WhileStmt:
			e := ast.(*WhileStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Cond)
			visit(e.Body)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *DoStmt:
			e := ast.(*DoStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			visit(e.Body)
			visit(e.Cond)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *DeclStmt:
			e := ast.(*DeclStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, stmt := range e.Decls {
				visit(stmt)
			}

			for _, d := range e.RecordDecls {
				visit(d)
			}

			for _, d := range e.EnumDecls {
				visit(d)
			}

			for _, d := range e.TypedefDecls {
				visit(d)
			}

			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ForStmt:
			e := ast.(*ForStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if e.Decl != nil {
				visit(e.Decl)
			} else {
				visit(e.Init)
			}
			visit(e.Cond)
			visit(e.Step)
			visit(e.Body)
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *GotoStmt:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *ContinueStmt:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *BreakStmt:
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		case *CompoundStmt:
			e := ast.(*CompoundStmt)
			if !tryCall(WalkerPropagate, ast) {
				return
			}
			for _, stmt := range e.Stmts {
				visit(stmt)
			}
			if !tryCall(WalkerBubbleUp, ast) {
				return
			}

		default:
			break
		}
	}

	visit(top)
}
