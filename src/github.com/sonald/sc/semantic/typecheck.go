package sema

import (
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/util"
)

// check types
// 1. type loop
// 2. type match
func (self *Parser) CheckTypes() {
}

// Check all DeclRef is a ref of the some defined
func (self *Parser) ReferenceResolve() {

	var walker = struct {
		WalkDeclRefExpr func(ws WalkStage, e *DeclRefExpr, ctx *ast.WalkContext) bool
	}{}

	walker.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if sym := ctx.Scope.LookupSymbol(e.Name); sym != nil {
			} else {
				// report error
				util.Printf(util.Sema, util.Info, "symbol %v does not defined", e.Name)
			}
		}
		return true
	}

	WalkAst(self.tu, walker)
}

func init() {
	fmt.Println("vim-go")
}
