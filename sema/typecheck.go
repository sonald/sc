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

// check types, type match, type conversion
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
		WalkMemberExpr           func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool
		WalkFunctionCall         func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext)
		WalkCompoundAssignExpr   func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext)
		WalkCastExpr             func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext)
		WalkCompoundLiteralExpr  func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext)
		WalkInitListExpr         func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext)
		WalkImplicitCastExpr     func(ws ast.WalkStage, e *ast.ImplicitCastExpr, ctx *ast.WalkContext)
		WalkVariableDecl         func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext)
		WalkFunctionDecl         func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext)
		WalkReturnStmt           func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext)
	}

	var info struct {
		LastFunction *ast.Symbol
	}

	// typedef's are handled elsewhere

	// this is integer promotion
	var promoteNode = func(expr ast.Expression, nd *ast.Node) ast.Expression {
		var ty = expr.GetType()
		if it, yes := ty.(*ast.IntegerType); yes {
			if it.Kind == "char" || it.Kind == "short" {
				var e = &ast.ImplicitCastExpr{}
				e.Node = *nd
				e.CastKind = ast.IntegralCast
				e.DestType = &ast.IntegerType{false, "int"}
				e.Expr = expr
				e.InferedType = e.DestType
				return e
			}
		}
		return expr
	}

	var tryImplicitCast = func(expr ast.Expression, destType ast.SymbolType, nd *ast.Node) ast.Expression {
		var ty = expr.GetType()
		// do conversion
		switch ty.(type) {
		case *ast.VoidType:
		case *ast.IntegerType:
			var e = &ast.ImplicitCastExpr{*nd, ast.IntegralCast, destType, expr}
			e.InferedType = e.DestType
			expr = e

		case *ast.FloatType, *ast.DoubleType:
			var e = &ast.ImplicitCastExpr{*nd, ast.FloatingToIntegralCast, destType, expr}
			e.InferedType = e.DestType
			expr = e

		case *ast.Array:
			var e = &ast.ImplicitCastExpr{*nd, ast.ArrayToPointerDecay, destType, expr}
			e.InferedType = e.DestType
			expr = e

		case *ast.Function:
			var e = &ast.ImplicitCastExpr{*nd, ast.FunctionToPointerDecay, destType, expr}
			e.InferedType = e.DestType
			expr = e

		default:
		}
		return expr

	}

	var unifyType = func(type1, type2 ast.SymbolType) (unified_ty ast.SymbolType, unified bool) {
		unified = true
		var t1, t2 = reflect.TypeOf(type1).Elem(), reflect.TypeOf(type2).Elem()
		if t1.Name() == "Pointer" || t2.Name() == "Pointer" {
			unified = false

		} else if t1.Name() == "DoubleType" || t2.Name() == "DoubleType" {
			if t2.Name() == "DoubleType" {
				type1, type2 = type2, type1
			}
			switch type2.(type) {
			case *ast.IntegerType, *ast.FloatType, *ast.DoubleType:
				unified_ty = &ast.DoubleType{}
			default:
				unified = false
			}

		} else if t1.Name() == "FloatType" || t2.Name() == "FloatType" {
			if t2.Name() == "FloatType" {
				type1, type2 = type2, type1
			}
			switch type2.(type) {
			case *ast.IntegerType, *ast.FloatType:
				unified_ty = &ast.FloatType{}
			default:
				unified = false
			}

		} else if t1.Name() == "IntegerType" && t2.Name() == "IntegerType" {
			var i1, i2 = type1.(*ast.IntegerType), type2.(*ast.IntegerType)

			// this is not the rank of C type, but more like privileges
			var ranks = map[string]int{
				"unsigned long long": 10,
				"long long":          9,
				"unsigned long":      8,
				"long":               7,
				"unsigned int":       6,
				"int":                5,
				// these below won't be used due to promotion
				"unsigned short": 4,
				"short":          3,
				"unsigned char":  2,
				"char":           1,
			}

			if i1.Kind == i2.Kind && i1.Unsigned == i2.Unsigned {
				unified_ty = type1

			} else {
				var ts1, ts2 = i1.String(), i2.String()
				if ranks[ts1] > ranks[ts2] {
					unified_ty = type1
				} else if ranks[ts1] < ranks[ts2] {
					unified_ty = type2
				}
			}

		} else {
			unified = false
		}
		return
	}

	var usualArithmeticConversion = func(node ast.Node, op lexer.Kind, lhs ast.Expression, rhs ast.Expression) (ast.Expression, ast.Expression, ast.SymbolType) {
		var lty, rty = lhs.GetType(), rhs.GetType()

		if ast.IsTypeEq(lty, rty) {
			return lhs, rhs, lty
		}

		var ty, unified = unifyType(lty, rty)
		if !unified {
			panic("invalid type for binary operation")
		}

		if !ast.IsTypeEq(lty, ty) {
			lhs = tryImplicitCast(lhs, ty, &node)
		}

		if !ast.IsTypeEq(rty, ty) {
			rhs = tryImplicitCast(rhs, ty, &node)
		}

		return lhs, rhs, ty
	}

	var checkPointerArithmetic = func(bop *ast.BinaryOperation) bool {
		var lty, rty = bop.LHS.GetType(), bop.RHS.GetType()

		var t1, t2 = reflect.TypeOf(lty).Elem(), reflect.TypeOf(rty).Elem()
		if t1.Name() == "Array" {
			var aty = lty.(*ast.Array)
			var dty = &ast.Pointer{aty.ElemType}
			bop.LHS = tryImplicitCast(bop.LHS, dty, &bop.Node)
			lty = dty
			t1 = reflect.TypeOf(lty).Elem()
		}

		if t2.Name() == "Array" {
			var aty = rty.(*ast.Array)
			var dty = &ast.Pointer{aty.ElemType}
			bop.RHS = tryImplicitCast(bop.RHS, dty, &bop.Node)
			rty = dty
			t2 = reflect.TypeOf(rty).Elem()
		}

		if t1.Name() == "Pointer" || t2.Name() == "Pointer" {
			if t1.Name() == "Pointer" && t2.Name() == "Pointer" {
				if bop.Op == lexer.PLUS {
					panic("invalid operands to pointer arithmetic")
				}

				//else, pointer subtraction results into a ptrdiff_t (signed int) type
				bop.InferedType = &ast.IntegerType{false, "int"}
				if !ast.IsTypeEq(lty, rty) {
					panic(fmt.Sprintf("'%s' and '%s' are not pointers to compatible types", lty, rty))
				}

			} else {
				if t2.Name() == "Pointer" {
					lty, rty = rty, lty
				}

				bop.InferedType = lty
				ast.TypeAssertCompat(rty, &ast.IntegerType{}, "invalid operand to pointer arithmetic")
			}
			return true
		}

		return false
	}

	var functionOrArrayConversion = func(e ast.Expression, node *ast.Node) ast.Expression {
		var ty = e.GetType()
		switch ty.(type) {
		case *ast.Function:
			ty = &ast.Pointer{ty.(*ast.Function)}
			e = tryImplicitCast(e, ty, node)
		case *ast.Array:
			ty = &ast.Pointer{ty.(*ast.Array).ElemType}
			e = tryImplicitCast(e, ty, node)
		}

		return e
	}

	CheckTypes.WalkVariableDecl = func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
			e.InferedType = sym.Type
			if e.Init != nil {
				if _, yes := e.Init.(*ast.InitListExpr); !yes {
					e.Init = functionOrArrayConversion(e.Init, &e.Node)

					if !ast.IsTypeEq(sym.Type, e.Init.GetType()) {
						e.Init = tryImplicitCast(e.Init, sym.Type, &e.Node)
					}
				}
			}
		}
	}

	CheckTypes.WalkIntLiteralExpr = func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = &ast.IntegerType{false, "int"}
		}
	}
	CheckTypes.WalkCharLiteralExpr = func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = &ast.IntegerType{false, "char"}
		}
	}
	CheckTypes.WalkStringLiteralExpr = func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = &ast.StringType{}
		}
	}

	CheckTypes.WalkBinaryOperation = func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			if e.Op == lexer.ASSIGN {
				e.RHS = functionOrArrayConversion(e.RHS, &e.Node)

				e.InferedType = e.LHS.GetType()
				if !ast.IsTypeEq(e.LHS.GetType(), e.RHS.GetType()) {
					e.RHS = tryImplicitCast(e.RHS, e.LHS.GetType(), &e.Node)
				}

			} else if e.Op == lexer.LOG_OR || e.Op == lexer.LOG_AND {
				e.InferedType = e.LHS.GetType()
				e.RHS = functionOrArrayConversion(e.RHS, &e.Node)

			} else if e.Op == lexer.COMMA {
				e.InferedType = e.LHS.GetType()
				e.RHS = functionOrArrayConversion(e.RHS, &e.Node)

			} else if e.Op == lexer.LSHIFT || e.Op == lexer.RSHIFT {
				e.LHS = promoteNode(e.LHS, &e.Node)
				e.RHS = promoteNode(e.RHS, &e.Node)
				e.InferedType = e.LHS.GetType()

			} else {
				e.LHS = functionOrArrayConversion(e.LHS, &e.Node)
				e.RHS = functionOrArrayConversion(e.RHS, &e.Node)

				if (e.Op == lexer.PLUS || e.Op == lexer.MINUS) && checkPointerArithmetic(e) {
					return
				}
				e.LHS = promoteNode(e.LHS, &e.Node)
				e.RHS = promoteNode(e.RHS, &e.Node)

				var ty ast.SymbolType
				e.LHS, e.RHS, ty = usualArithmeticConversion(e.Node, e.Op, e.LHS, e.RHS)
				util.Printf(util.Sema, util.Debug, "unified ty %v", ty)
				e.InferedType = ty

			}
		}
	}
	CheckTypes.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			// we do not need to consider other namespace since it'll be taken care
			// elsewhere. e.g MemberExpr
			util.Printf(util.Sema, util.Debug, "lookup %s", e.Name)
			var sym = ctx.Scope.LookupSymbol(e.Name, ast.OrdinaryNS)
			e.InferedType = sym.Type
		}
	}
	CheckTypes.WalkUnaryOperation = func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			switch e.Op {
			case lexer.INC, lexer.DEC:
				e.InferedType = e.Expr.GetType()

			case lexer.MINUS, lexer.PLUS:
				e.InferedType = e.Expr.GetType()
				ast.TypeAssertCompat(e.Expr.GetType(), &ast.IntegerType{}, "types are uncompatible")

			case lexer.NOT, lexer.TILDE:
				e.InferedType = &ast.IntegerType{false, "int"}
				ast.TypeAssertCompat(e.Expr.GetType(), &ast.IntegerType{}, "types are uncompatible")

			case lexer.AND:
				e.InferedType = &ast.Pointer{e.Expr.GetType()}

			case lexer.MUL: // pointer deref
				e.Expr = functionOrArrayConversion(e.Expr, &e.Node)

				var ty = e.Expr.GetType()
				if _, ok := ty.(*ast.Pointer); !ok {
					panic("indirection requires pointer operand")
				} else {
					e.InferedType = ty.(*ast.Pointer).Source
				}

			default:
				panic("wrong unary operator")
			}
		}
	}
	CheckTypes.WalkSizeofExpr = func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			//FIXME: this is impl-defined (now set as size_t for x86_64)
			e.InferedType = &ast.IntegerType{true, "int"}
		}
	}
	CheckTypes.WalkConditionalOperation = func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.Cond = functionOrArrayConversion(e.Cond, &e.Node)
			e.True = functionOrArrayConversion(e.True, &e.Node)
			e.False = functionOrArrayConversion(e.False, &e.Node)

			e.InferedType = e.True.GetType()
			ast.TypeAssertEq(e.False.GetType(), e.True.GetType(), "conditional type mismatch")
		}
	}
	CheckTypes.WalkArraySubscriptExpr = func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			var ty = e.Target.GetType()
			if aty, yes := ty.(*ast.Array); !yes {
				panic("target should be array type")
			} else {
				e.InferedType = aty.ElemType
				util.Printf(util.Sema, util.Debug, "WalkArraySubscriptExpr %v", e.InferedType)
			}

			if _, yes := e.Sub.GetType().(*ast.IntegerType); !yes {
				panic("subscript should be integer type")
			}
		}
	}
	CheckTypes.WalkMemberExpr = func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if decl, yes := e.Target.(*ast.DeclRefExpr); yes {
				var sym = ctx.Scope.LookupRecordVar(decl.Name)
				if sym == nil {
					panic("invalid record reference")
				} else {
					var ty = sym.Type
					if _, ok := ty.(*ast.Pointer); ok {
						ty = ty.(*ast.Pointer).Source
					}
					var rdty = ty.(*ast.RecordType)
					util.Printf(util.Sema, util.Debug, "MemberExpr: found rdty %v for %s", rdty, decl.Name)
					decl.InferedType = rdty
				}
			} else {
				ast.WalkAst(e.Target, CheckTypes, ctx)
			}

			if decl, yes := e.Member.(*ast.DeclRefExpr); yes {
				var found = false
				if rdty, yes := e.Target.GetType().(*ast.RecordType); yes {
					for _, fty := range rdty.Fields {
						if fty.Name == decl.Name {
							decl.InferedType = fty.Base
							e.InferedType = decl.InferedType
							found = true
							break
						}
					}

					if !found {
						panic("invalid member")
					}
				} else {
					panic("invalid member reference")
				}

			} else {
				panic("invalid member reference")
			}

			return false
		}
		return true
	}
	CheckTypes.WalkFunctionDecl = func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			sym := ctx.Scope.LookupSymbol(e.Name, ast.OrdinaryNS)
			info.LastFunction = sym
		} else {
			info.LastFunction = nil
		}
	}
	CheckTypes.WalkReturnStmt = func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			var funReturnType ast.SymbolType
			if fty, yes := info.LastFunction.Type.(*ast.Function); yes {
				funReturnType = fty.Return
			}
			if e.Expr != nil && !ast.IsTypeEq(funReturnType, e.Expr.GetType()) {
				e.Expr = tryImplicitCast(e.Expr, funReturnType, &e.Node)
			}
		}
	}
	CheckTypes.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.Func = functionOrArrayConversion(e.Func, &e.Node)

			var ty = e.Func.GetType()
		done:
			for {
				switch ty.(type) {
				case *ast.Pointer:
					ty = ty.(*ast.Pointer).Source
				default:
					break done
				}
			}
			if ty, yes := ty.(*ast.Function); yes {
				e.InferedType = ty.Return
				if len(ty.Args) != len(e.Args) {
					switch e.Func.(type) {
					case *ast.ArraySubscriptExpr:
						var howmany = "many"
						if len(ty.Args) > len(e.Args) {
							howmany = "few"
						}
						panic(fmt.Sprintf("too %s arguments to function call, expect %d, have %d",
							howmany, len(ty.Args), len(e.Args)))
					case *ast.DeclRefExpr:
						panic(fmt.Sprintf("no matching function for call to '%s'", e.Func.(*ast.DeclRefExpr).Name))
					}
				} else {
					for i := 0; i < len(ty.Args); i++ {
						if !ast.IsTypeEq(ty.Args[i], e.Args[i].GetType()) {
							if ast.IsTypeCompat(ty.Args[i], e.Args[i].GetType()) {
								e.Args[i] = tryImplicitCast(e.Args[i], ty.Args[i], &e.Node)
							}
						}
					}
				}
			} else {
				panic("invalid function type")
			}
		}
	}
	CheckTypes.WalkCompoundAssignExpr = func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = e.LHS.GetType()
			ast.TypeAssertEq(e.LHS.GetType(), e.RHS.GetType(), "type mismatch")
		}
	}
	CheckTypes.WalkCastExpr = func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = e.Type
		}
	}
	CheckTypes.WalkCompoundLiteralExpr = func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = e.Type
		}
	}
	CheckTypes.WalkInitListExpr = func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			for i := 0; i < len(e.Inits); i++ {
				e.Inits[i] = functionOrArrayConversion(e.Inits[i], &e.Node)
			}
			// initList itself can not determine type
			e.InferedType = nil
		}
	}
	CheckTypes.WalkImplicitCastExpr = func(ws ast.WalkStage, e *ast.ImplicitCastExpr, ctx *ast.WalkContext) {
		if ws == ast.WalkerBubbleUp {
			e.InferedType = e.DestType
		}
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
					var sym = ctx.Scope.LookupRecordVar(e.Name)
					if sym == nil {
						addReport(ast.Error, e.Start, fmt.Sprintf(err2, e.Name))
					} else {
						var ty = sym.Type
						if _, ok := ty.(*ast.Pointer); ok {
							ty = ty.(*ast.Pointer).Source
						}
						var rdty = ty.(*ast.RecordType)
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
