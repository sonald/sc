package sema

import (
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"reflect"
)

var (
	err1      = "implicit declaration of function '%s' is invalid in C99"
	err2      = "use of undeclared identifier '%s'"
	err3      = "field has incomplete type '%s'"
	err4      = "field has incomplete type 'my_t' (aka 'struct node2')"
	reports   []*ast.Report
	addReport = func(kd ast.ReportKind, tk lexer.Token, desc string) {
		reports = append(reports, &ast.Report{kd, tk, desc})
	}

	isFuncCall = false
)

// 1. type loop
func MakeCheckLoop() ast.AstWalker {
	type Node struct {
		st    ast.SymbolType
		Start lexer.Token
		Next  *Node
	}

	type Graph struct {
		nodes map[ast.SymbolType]*Node
	}

	var g = Graph{make(map[ast.SymbolType]*Node)}
	var cur ast.SymbolType
	var start lexer.Token

	var addEdge = func(u, v ast.SymbolType, tok lexer.Token) {
		util.Printf(util.Sema, util.Info, fmt.Sprintf("AddEdge(%s, %s)\n", u, v))
		if _, ok := g.nodes[u]; !ok {
			g.nodes[u] = &Node{st: u, Start: start}
		}

		for p := g.nodes[u].Next; p != nil; p = p.Next {
			if p.st == v {
				return
			}
		}

		var n = &Node{st: v, Start: tok}
		n.Next = g.nodes[u].Next
		g.nodes[u].Next = n
	}

	var detectLoop = func() {
		var visited = make(map[ast.SymbolType]bool)

		var dfs = func(u ast.SymbolType) {
			for v := g.nodes[u].Next; v != nil; v = v.Next {
				if visited[v.st] {
					addReport(ast.Error, v.Start, fmt.Sprintf(err3, v.st))
				}
			}
		}

		for _, n := range g.nodes {
			if !visited[n.st] {
				visited[n.st] = true
				dfs(n.st)
			}
		}
	}

	var CheckLoop struct {
		WalkTranslationUnit func(ws ast.WalkStage, e *ast.TranslationUnit, ctx *ast.WalkContext)
		WalkFieldDecl       func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext)
		WalkRecordDecl      func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext)
	}

	CheckLoop.WalkTranslationUnit = func(ws ast.WalkStage, e *ast.TranslationUnit, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			for u, p := range g.nodes {
				for v := p.Next; v != nil; v = v.Next {
					util.Printf(util.Sema, util.Debug, fmt.Sprintf("%s -> %s\n", u, v.st))
				}
			}
			detectLoop()
			for _, r := range reports {
				fmt.Printf("error: %v\n", fmt.Sprintf("%d:%d, %s", r.Line, r.Column, r.Desc))
			}
		}
	}

	CheckLoop.WalkFieldDecl = func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
			switch sym.Type.(type) {
			case *ast.RecordType, *ast.EnumType:
				addEdge(cur, sym.Type, e.Start)
			}
		}
	}
	CheckLoop.WalkRecordDecl = func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.TagNS)
			cur = sym.Type
			start = e.Start
		}
	}

	return CheckLoop
}

// check types, type match
func MakeCheckTypes() ast.AstWalker {
	var CheckTypes struct {
		WalkIntLiteralExpr       func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext)
		WalkCharLiteralExpr      func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext)
		WalkStringLiteralExpr    func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext)
		WalkBinaryOperation      func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext)
		WalkDeclRefExpr          func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext)
		WalkUnaryOperation       func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext)
		WalkSizeofExpr           func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext)
		WalkConditionalOperation func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext)
		WalkArraySubscriptExpr   func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext)
		WalkMemberExpr           func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext)
		WalkFunctionCall         func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext)
		WalkCompoundAssignExpr   func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext)
		WalkCastExpr             func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext)
		WalkCompoundLiteralExpr  func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext)
		WalkInitListExpr         func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext)
	}

	CheckTypes.WalkIntLiteralExpr = func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkCharLiteralExpr = func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkStringLiteralExpr = func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkBinaryOperation = func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkUnaryOperation = func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkSizeofExpr = func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkConditionalOperation = func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkArraySubscriptExpr = func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkMemberExpr = func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkCompoundAssignExpr = func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkCastExpr = func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkCompoundLiteralExpr = func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) {
	}
	CheckTypes.WalkInitListExpr = func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) {
	}
	return CheckTypes
}

// Check all DeclRef is a ref of some of the defineds
func MakeReferenceResolve() ast.AstWalker {

	var (
		isFuncCall = false
	)

	var referenceResolve struct {
		WalkTranslationUnit func(ws ast.WalkStage, e *ast.TranslationUnit, ctx *ast.WalkContext)
		WalkDeclRefExpr     func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool
		WalkFunctionCall    func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool
	}

	referenceResolve.WalkTranslationUnit = func(ws ast.WalkStage, e *ast.TranslationUnit, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			for _, r := range reports {
				fmt.Printf("error: %v\n", fmt.Sprintf("%d:%d, %s", r.Line, r.Column, r.Desc))
			}
		}
	}

	referenceResolve.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if sym := ctx.Scope.LookupSymbol(e.Name, ast.AnyNS); sym != nil {
			} else {
				if isFuncCall {
					addReport(ast.Error, e.Start, fmt.Sprintf(err1, e.Name))
				} else {
					addReport(ast.Error, e.Start, fmt.Sprintf(err2, e.Name))
				}
			}
		} else {
			if isFuncCall {
				isFuncCall = false
			}
		}
		return true
	}

	referenceResolve.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			isFuncCall = true
		}
		return true
	}

	return referenceResolve
}

var walkers []ast.AstWalker

func RunWalkers(top ast.Ast) {
	for _, w := range walkers {
		util.Printf("run walker: %v\n", reflect.TypeOf(w))
		ast.WalkAst(top, w)
		util.Printf("done walker: %v\n", reflect.TypeOf(w))
	}
}

func init() {
	walkers = []ast.AstWalker{MakeReferenceResolve(), MakeCheckLoop(), MakeCheckTypes()}
}
