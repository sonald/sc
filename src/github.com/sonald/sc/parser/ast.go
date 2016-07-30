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
}

type Node struct {
	ctx *AstContext
}

func (n *Node) Repr() string {
	return "#"
}

type TranslationUnit struct {
	Node
	filename    string
	funcDecls   []*FunctionDecl
	varDecls    []*VariableDecl
	recordDecls []*RecordDecl
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
	inits []Expression
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
	Sym    string
	Fields []*FieldDecl
	Loc    lexer.Location
	Scope  *SymbolScope
}

func (self *RecordDecl) Repr() string {
	return fmt.Sprintf("RecordDecl(%s)", self.Sym)
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
	Decls       []*VariableDecl
	RecordDecls []*RecordDecl
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

type AstWalker interface {
}

func WalkAst(top Ast, wk AstWalker) {
	var wkValue = reflect.ValueOf(wk)
	var visit func(ast Ast)

	var tryCall = func(stage WalkStage, ast Ast) {
		var method = wkValue.FieldByName("Walk" + reflect.TypeOf(ast).Elem().Name())
		if method.IsValid() {
			method.Call([]reflect.Value{reflect.ValueOf(stage), reflect.ValueOf(ast)})
		}
	}

	visit = func(ast Ast) {
		switch ast.(type) {
		case *TranslationUnit:
			tu := ast.(*TranslationUnit)
			tryCall(WalkerPropagate, ast)
			for _, d := range tu.varDecls {
				visit(d)
			}

			for _, d := range tu.recordDecls {
				visit(d)
			}

			for _, d := range tu.funcDecls {
				visit(d)
			}
			tryCall(WalkerBubbleUp, ast)

		case *IntLiteralExpr:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *CharLiteralExpr:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *StringLiteralExpr:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *BinaryOperation:
			var e = ast.(*BinaryOperation)
			tryCall(WalkerPropagate, ast)
			visit(e.LHS)
			visit(e.RHS)
			tryCall(WalkerBubbleUp, ast)

		case *DeclRefExpr:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *UnaryOperation:
			var e = ast.(*UnaryOperation)
			tryCall(WalkerPropagate, ast)
			visit(e.expr)
			tryCall(WalkerBubbleUp, ast)

		case *ConditionalOperation:
			e := ast.(*ConditionalOperation)
			tryCall(WalkerPropagate, ast)
			visit(e.Cond)
			visit(e.True)
			visit(e.False)
			tryCall(WalkerBubbleUp, ast)

		case *ArraySubscriptExpr:
			e := ast.(*ArraySubscriptExpr)
			tryCall(WalkerPropagate, ast)
			visit(e.Target)
			visit(e.Sub)
			tryCall(WalkerBubbleUp, ast)

		case *MemberExpr:
			e := ast.(*MemberExpr)
			tryCall(WalkerPropagate, ast)
			visit(e.Target)
			visit(e.Member)
			tryCall(WalkerBubbleUp, ast)

		case *FunctionCall:
			e := ast.(*FunctionCall)
			tryCall(WalkerPropagate, ast)
			visit(e.Func)
			for _, arg := range e.Args {
				visit(arg)
			}
			tryCall(WalkerBubbleUp, ast)

		case *CompoundAssignExpr:
			var e = ast.(*CompoundAssignExpr)
			tryCall(WalkerPropagate, ast)
			visit(e.LHS)
			visit(e.RHS)
			tryCall(WalkerBubbleUp, ast)

		case *CastExpr:
			e := ast.(*CastExpr)
			tryCall(WalkerPropagate, ast)
			visit(e.Expr)
			tryCall(WalkerBubbleUp, ast)

		case *CompoundLiteralExpr:
			e := ast.(*CompoundLiteralExpr)
			tryCall(WalkerPropagate, ast)
			visit(e.InitList)
			tryCall(WalkerBubbleUp, ast)

		case *InitListExpr:
			e := ast.(*InitListExpr)
			tryCall(WalkerPropagate, ast)
			for _, init := range e.inits {
				visit(init)
			}
			tryCall(WalkerBubbleUp, ast)

		case *FieldDecl:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *RecordDecl:
			e := ast.(*RecordDecl)
			tryCall(WalkerPropagate, ast)
			for _, f := range e.Fields {
				visit(f)
			}
			tryCall(WalkerBubbleUp, ast)

		case *VariableDecl:
			e := ast.(*VariableDecl)
			tryCall(WalkerPropagate, ast)
			if e.init != nil {
				visit(e.init)
			}
			tryCall(WalkerBubbleUp, ast)

		case *ParamDecl:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *FunctionDecl:
			e := ast.(*FunctionDecl)
			tryCall(WalkerPropagate, ast)
			for _, arg := range e.Args {
				visit(arg)
			}

			if e.Body != nil {
				visit(e.Body)
			}
			tryCall(WalkerBubbleUp, ast)

		case *ExprStmt:
			e := ast.(*ExprStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Expr)
			tryCall(WalkerBubbleUp, ast)

		case *LabelStmt:
			e := ast.(*LabelStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Stmt)
			tryCall(WalkerBubbleUp, ast)

		case *CaseStmt:
			e := ast.(*CaseStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.ConstExpr)
			visit(e.Stmt)
			tryCall(WalkerBubbleUp, ast)

		case *DefaultStmt:
			e := ast.(*DefaultStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Stmt)
			tryCall(WalkerBubbleUp, ast)

		case *ReturnStmt:
			e := ast.(*ReturnStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Expr)
			tryCall(WalkerBubbleUp, ast)

		case *IfStmt:
			e := ast.(*IfStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Cond)
			if e.TrueBranch != nil {
				visit(e.TrueBranch)
			}
			if e.FalseBranch != nil {
				visit(e.FalseBranch)
			}
			tryCall(WalkerBubbleUp, ast)

		case *SwitchStmt:
			e := ast.(*SwitchStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Cond)
			visit(e.Body)
			tryCall(WalkerBubbleUp, ast)

		case *WhileStmt:
			e := ast.(*WhileStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Cond)
			visit(e.Body)
			tryCall(WalkerBubbleUp, ast)

		case *DoStmt:
			e := ast.(*DoStmt)
			tryCall(WalkerPropagate, ast)
			visit(e.Body)
			visit(e.Cond)
			tryCall(WalkerBubbleUp, ast)

		case *DeclStmt:
			e := ast.(*DeclStmt)
			tryCall(WalkerPropagate, ast)
			for _, stmt := range e.Decls {
				visit(stmt)
			}
			tryCall(WalkerBubbleUp, ast)

		case *ForStmt:
			e := ast.(*ForStmt)
			tryCall(WalkerPropagate, ast)
			if e.Decl != nil {
				visit(e.Decl)
			} else {
				visit(e.Init)
			}
			visit(e.Cond)
			visit(e.Step)
			visit(e.Body)
			tryCall(WalkerBubbleUp, ast)

		case *GotoStmt:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *ContinueStmt:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *BreakStmt:
			tryCall(WalkerPropagate, ast)
			tryCall(WalkerBubbleUp, ast)

		case *CompoundStmt:
			e := ast.(*CompoundStmt)
			tryCall(WalkerPropagate, ast)
			for _, stmt := range e.Stmts {
				visit(stmt)
			}
			tryCall(WalkerBubbleUp, ast)

		default:
			break
		}
	}

	visit(top)
}
