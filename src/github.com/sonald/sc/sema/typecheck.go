package sema

import (
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"reflect"
	"sort"
)

var (
	err1      = "implicit declaration of function '%s' is invalid in C99"
	err2      = "use of undeclared identifier '%s'"
	err3      = "field '%s' has incomplete type '%s'"
	err4      = "field '%s' has incomplete type '%s' (aka '%s')"
	err5      = "no member named '%s' in '%s'"
	Reports   []*ast.Report
	addReport = func(kd ast.ReportKind, tk lexer.Token, desc string) {
		Reports = append(Reports, &ast.Report{kd, tk, desc})
	}

	isFuncCall = false
)

// 1. type loop
func MakeCheckLoop() ast.AstWalker {
	type Node struct {
		st    ast.SymbolType
		Start lexer.Token
		Ref   int
		Next  *Node
	}

	type Graph struct {
		nodes map[ast.SymbolType]*Node
	}

	var g = Graph{make(map[ast.SymbolType]*Node)}
	var cur ast.SymbolType
	var start lexer.Token

	var addEdge = func(u, v ast.SymbolType, tok lexer.Token) {
		util.Printf(util.Sema, util.Info, fmt.Sprintf("AddEdge(%s, %s:%s)\n", u, tok.AsString(), v))
		if _, ok := g.nodes[u]; !ok {
			g.nodes[u] = &Node{st: u, Start: start, Ref: 1}
		} else {
			g.nodes[u].Ref++
		}

		var n = &Node{st: v, Start: tok, Ref: 1}
		n.Next = g.nodes[u].Next
		g.nodes[u].Next = n
	}

	var detectLoop = func() {
		var visited = make(map[ast.SymbolType]bool)

		var dfs = func(u ast.SymbolType) {
			for v := g.nodes[u].Next; v != nil; v = v.Next {
				if visited[v.st] && v.Ref > 0 {
					addReport(ast.Error, v.Start, fmt.Sprintf(err3, v.Start.AsString(), v.st))
					v.Ref--
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
			for _, p := range g.nodes {
				for v := p.Next; v != nil; v = v.Next {
					util.Printf(util.Sema, util.Debug, fmt.Sprintf("(%s, %s) -> (%s, %s)\n",
						p.Start.AsString(), p.st, v.Start.AsString(), v.st))
				}
			}
			detectLoop()
		}
	}

	CheckLoop.WalkFieldDecl = func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
			switch sym.Type.(type) {
			case *ast.RecordType, *ast.EnumType:
				addEdge(cur, sym.Type, e.Start)

			case *ast.UserType:
				ut := sym.Type.(*ast.UserType)
				switch ut.Ref.(type) {
				case *ast.RecordType, *ast.EnumType:
					addEdge(cur, ut.Ref, e.Start)
				}
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

	const (
		CNormal int = iota
		CIsFuncCall
		CSearchRecord
		CSearchRecordMember
	)

	var (
		state  = CNormal
		rdName string
	)

	var referenceResolve struct {
		WalkDeclRefExpr  func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool
		WalkFunctionCall func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool
		WalkMemberExpr   func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool
	}

	referenceResolve.WalkMemberExpr = func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			state = CSearchRecord
			ast.WalkAst(e.Target, referenceResolve, ctx)
			state = CSearchRecordMember
			ast.WalkAst(e.Member, referenceResolve, ctx)
			state = CNormal
			rdName = ""
			return false
		}
		return true
	}

	referenceResolve.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			switch state {
			case CSearchRecord, CSearchRecordMember:
				if state == CSearchRecord {
					var isARecordWithName = func(sym *ast.Symbol) bool {
						if _, ok := sym.Type.(*ast.RecordType); ok && sym.Name.AsString() == e.Name {
							return true
						}
						return false
					}
					var syms = ctx.Scope.LookupSymbolsBy(isARecordWithName)
					if syms == nil {
						addReport(ast.Error, e.Start, fmt.Sprintf(err2, e.Name))
					} else {
						var rdty = syms[0].Type.(*ast.RecordType)
						rdName = rdty.Name
					}

				} else {
					if len(rdName) > 0 {
						util.Printf("check member %s for %s\n", e.Name, rdName)
						var rdty = ctx.Scope.LookupNamedTypeRecursive(rdName, ast.TagNS).(*ast.RecordType)
						var meetMember = false
						for _, fld := range rdty.Fields {
							if fld.Name == e.Name {
								meetMember = true
								break
							}
						}

						if !meetMember {
							addReport(ast.Error, e.Start, fmt.Sprintf(err5, e.Name, "record "+rdName))
						}
					}
				}

			default:
				if sym := ctx.Scope.LookupSymbol(e.Name, ast.AnyNS); sym == nil {
					if state == CIsFuncCall {
						addReport(ast.Error, e.Start, fmt.Sprintf(err1, e.Name))
					} else {
						addReport(ast.Error, e.Start, fmt.Sprintf(err2, e.Name))
					}
				}
			}
		} else {
			if state == CIsFuncCall {
				state = CNormal
			}
		}
		return true
	}

	referenceResolve.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			state = CIsFuncCall
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

type byPos []*ast.Report

func (a byPos) Len() int      { return len(a) }
func (a byPos) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a byPos) Less(i, j int) bool {
	return a[i].Line < a[j].Line || (a[i].Line == a[j].Line && a[i].Column < a[j].Column)
}

func DumpReports() {
	sort.Stable(byPos(Reports))
	for _, r := range Reports {
		fmt.Printf("error: %v\n", fmt.Sprintf("%d:%d, %s", r.Line, r.Column, r.Desc))
	}
}

func init() {
	walkers = []ast.AstWalker{MakeReferenceResolve(), MakeCheckLoop(), MakeCheckTypes()}
}
