package sema

import (
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"reflect"
)

// check types
// 1. type loop
// 2. type match
var CheckTypes = struct {
}{}

// Check all DeclRef is a ref of some of the defineds
func MakeReferenceResolve() ast.AstWalker {

	var (
		err1      = "implicit declaration of function '%s' is invalid in C99"
		err2      = "use of undeclared identifier '%s'"
		reports   []*ast.Report
		addReport = func(kd ast.ReportKind, tk lexer.Token, desc string) {
			reports = append(reports, &ast.Report{kd, tk, desc})
		}

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
			if sym := ctx.Scope.LookupSymbol(e.Name, false); sym != nil {
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
	walkers = []ast.AstWalker{MakeReferenceResolve(), CheckTypes}
}
