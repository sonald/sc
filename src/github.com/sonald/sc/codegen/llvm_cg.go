package codegen

import (
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/lexer"
	"github.com/sonald/sc/util"
	"llvm.org/llvm/bindings/go/llvm"
	"os"
)

func symbolTy2llvmType(st ast.SymbolType) (ret llvm.Type) {
	switch st.(type) {
	case *ast.IntegerType:
		ity := st.(*ast.IntegerType)
		//FIXME: consider x86-64 only now
		switch ity.Kind {
		case "char":
			ret = llvm.Int8Type()
		case "short":
			ret = llvm.Int16Type()
		case "int":
			ret = llvm.Int32Type()
		case "long":
			ret = llvm.Int64Type()
		case "long long":
			ret = llvm.Int64Type()
		}

	case *ast.VoidType:
		ret = llvm.VoidType()

	case *ast.FloatType:
		ret = llvm.FloatType()

	case *ast.DoubleType:
		ret = llvm.DoubleType()

	case *ast.Pointer:
		pty := st.(*ast.Pointer)
		ret = llvm.PointerType(symbolTy2llvmType(pty.Source), 0)

	case *ast.Array:
		aty := st.(*ast.Array)
		ret = symbolTy2llvmType(aty.ElemType)

		for _, e := range aty.LenExprs {
			ile := e.(*ast.IntLiteralExpr) // might fail
			if ile.Tok.AsInt() == -1 {
				ret = llvm.PointerType(ret, 0)
			} else {
				ret = llvm.ArrayType(ret, ile.Tok.AsInt())
			}
		}

	case *ast.Function:
		var ptys []llvm.Type
		fty := st.(*ast.Function)
		ll_rty := symbolTy2llvmType(fty.Return)

		for _, arg := range fty.Args {
			ptys = append(ptys, symbolTy2llvmType(arg))
		}

		ret = llvm.FunctionType(ll_rty, ptys, false)

	//case *ast.RecordType:

	default:
		util.Printf(util.CodeGen, util.Warning, "not implemented")
	}

	return
}

func MakeLLVMCodeGen() ast.AstWalker {
	type SwitchState struct {
		switch_val  llvm.Value // if non-nil, inside switch statement
		default_bb  llvm.BasicBlock
		cases       []llvm.BasicBlock
		count_cases bool // if true, count cases recursively
		num_cases   int
		has_default bool
		Next        *SwitchState
	}

	type Info struct {
		Mod     llvm.Module
		llvmCtx llvm.Context
		builder llvm.Builder
		top     *ast.TranslationUnit
		symbols []llvm.Value               // in order
		breaks  []llvm.BasicBlock          // stack of targets for `break` statement
		labels  map[string]llvm.BasicBlock // list of targets for `goto` statement
		sw      *SwitchState               // the innermost of switch states
	}

	var walker struct {
		Info

		WalkTranslationUnit      func(ast.WalkStage, *ast.TranslationUnit, *ast.WalkContext)
		WalkIntLiteralExpr       func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) bool
		WalkCharLiteralExpr      func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) bool
		WalkStringLiteralExpr    func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) bool
		WalkBinaryOperation      func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) bool
		WalkDeclRefExpr          func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool
		WalkUnaryOperation       func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) bool
		WalkSizeofExpr           func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) bool
		WalkConditionalOperation func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) bool
		WalkArraySubscriptExpr   func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) bool
		WalkMemberExpr           func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool
		WalkFunctionCall         func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool
		WalkCompoundAssignExpr   func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) bool
		WalkCastExpr             func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) bool
		WalkCompoundLiteralExpr  func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) bool
		WalkInitListExpr         func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) bool
		WalkFieldDecl            func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext)
		WalkRecordDecl           func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext)
		WalkEnumeratorDecl       func(ws ast.WalkStage, e *ast.EnumeratorDecl, ctx *ast.WalkContext)
		WalkEnumDecl             func(ws ast.WalkStage, e *ast.EnumDecl, ctx *ast.WalkContext)
		WalkVariableDecl         func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext) bool
		WalkTypedefDecl          func(ws ast.WalkStage, e *ast.TypedefDecl, ctx *ast.WalkContext)
		WalkParamDecl            func(ws ast.WalkStage, e *ast.ParamDecl, ctx *ast.WalkContext)
		WalkFunctionDecl         func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext)
		WalkDeclStmt             func(ws ast.WalkStage, e *ast.DeclStmt, ctx *ast.WalkContext) bool
		WalkExprStmt             func(ws ast.WalkStage, e *ast.ExprStmt, ctx *ast.WalkContext) bool
		WalkLabelStmt            func(ws ast.WalkStage, e *ast.LabelStmt, ctx *ast.WalkContext)
		WalkGotoStmt             func(ws ast.WalkStage, e *ast.GotoStmt, ctx *ast.WalkContext)
		WalkCaseStmt             func(ws ast.WalkStage, e *ast.CaseStmt, ctx *ast.WalkContext) bool
		WalkDefaultStmt          func(ws ast.WalkStage, e *ast.DefaultStmt, ctx *ast.WalkContext) bool
		WalkReturnStmt           func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext) bool
		WalkIfStmt               func(ws ast.WalkStage, e *ast.IfStmt, ctx *ast.WalkContext) bool
		WalkSwitchStmt           func(ws ast.WalkStage, e *ast.SwitchStmt, ctx *ast.WalkContext) bool
		WalkWhileStmt            func(ws ast.WalkStage, e *ast.WhileStmt, ctx *ast.WalkContext) bool
		WalkDoStmt               func(ws ast.WalkStage, e *ast.DoStmt, ctx *ast.WalkContext) bool
		WalkForStmt              func(ws ast.WalkStage, e *ast.ForStmt, ctx *ast.WalkContext) bool
		WalkContinueStmt         func(ws ast.WalkStage, e *ast.ContinueStmt, ctx *ast.WalkContext)
		WalkBreakStmt            func(ws ast.WalkStage, e *ast.BreakStmt, ctx *ast.WalkContext)
		WalkCompoundStmt         func(ws ast.WalkStage, e *ast.CompoundStmt, ctx *ast.WalkContext)
	}

	var log = func(f string, v ...interface{}) {
		if len(os.Getenv("DEBUG")) != 0 {
			fmt.Fprintf(os.Stderr, f, v...)
		}
	}

	var Append = func(v llvm.Value) {
		if v.IsNil() {
			log("Append nil\n")
		} else {
			log("Append %v\n", v.Name())
		}
		walker.Info.symbols = append(walker.Info.symbols, v)
	}

	var Drop = func() {
		var st = walker.Info.symbols
		var n int
		for n = len(st) - 1; n >= 0; n-- {
			if st[n].IsNil() {
				break
			}
		}
		log("Drop  %d -> %d\n", len(st), n)
		walker.Info.symbols = st[:n]
	}

	var Find = func(nm string) llvm.Value {
		log("Find(%s)\n", nm)
		var st = walker.Info.symbols
		for n := len(st) - 1; n >= 0; n-- {
			if !st[n].IsNil() && st[n].Name() == nm {
				return st[n]
			}
		}

		panic("Find failed")
		return llvm.Value{}
	}

	var AddLabel = func(nm string, bb llvm.BasicBlock) {
		if walker.Info.labels == nil {
			walker.Info.labels = make(map[string]llvm.BasicBlock)
		}
		log("AddLabel(%s)\n", nm)
		if _, ok := walker.Info.labels[nm]; !ok {
			walker.Info.labels[nm] = bb
		} else {
			panic(fmt.Sprintf("label %s exists", nm))
		}
	}

	var FindLabel = func(nm string) llvm.BasicBlock {
		log("FindLabel(%s)\n", nm)
		if walker.Info.labels == nil {
			walker.Info.labels = make(map[string]llvm.BasicBlock)
		}
		if _, ok := walker.Info.labels[nm]; ok {
			return walker.Info.labels[nm]
		}

		return llvm.BasicBlock{}
	}

	var DropAllLabels = func() {
		walker.Info.labels = nil
	}

	var PushBreak = func(bb llvm.BasicBlock) {
		walker.Info.breaks = append(walker.Info.breaks, bb)
	}

	var PopBreak = func() {
		var brs = walker.Info.breaks
		walker.Info.breaks = brs[:len(brs)-1]
	}

	var GetBreak = func() llvm.BasicBlock {
		return walker.Info.breaks[len(walker.Info.breaks)-1]
	}

	var PushSwitchState = func() *SwitchState {
		var ss = &SwitchState{}

		ss.Next = walker.Info.sw
		walker.Info.sw = ss
		return ss
	}

	var PopSwitchState = func() {
		if walker.Info.sw != nil {
			walker.Info.sw = walker.Info.sw.Next
		}
	}

	var InSwitchCaseCounting = func() bool {
		if walker.Info.sw != nil && walker.Info.sw.count_cases {
			return true
		}
		return false
	}

	walker.WalkTranslationUnit = func(ws ast.WalkStage, tu *ast.TranslationUnit, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			util.Printf("generated code\n")
			walker.Info.Mod = llvm.NewModule(tu.Filename)
			walker.Info.llvmCtx = walker.Info.Mod.Context()
			walker.Info.top = tu
			walker.Info.builder = llvm.NewBuilder()

		} else {
			walker.Info.Mod.Dump()
			ctx.Value = walker.Info.Mod
		}
	}

	walker.WalkIntLiteralExpr = func(ws ast.WalkStage, e *ast.IntLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			ctx.Value = llvm.ConstInt(walker.Info.llvmCtx.Int32Type(), uint64(e.Tok.AsInt()), false)
			//util.Printf("IntLiteralExpr: %s, int %s\n", ctx.Value.(llvm.Value),
			//ctx.Value.(llvm.Value).IsAConstantInt())
		}
		return true
	}

	walker.WalkCharLiteralExpr = func(ws ast.WalkStage, e *ast.CharLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		}
		return true
	}
	walker.WalkStringLiteralExpr = func(ws ast.WalkStage, e *ast.StringLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		}
		return true
	}

	walker.WalkBinaryOperation = func(ws ast.WalkStage, e *ast.BinaryOperation, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}
		if ws == ast.WalkerPropagate {
			var lhs, rhs, op llvm.Value

			//log("WalkBinaryOperation: lhs %v, rhs %v\n", lhs.Type(), rhs.Type())

			var ariths = map[lexer.Kind]func(lhs, rhs llvm.Value, name string) llvm.Value{
				lexer.PLUS:  walker.Info.builder.CreateAdd,
				lexer.MINUS: walker.Info.builder.CreateSub,
				lexer.MUL:   walker.Info.builder.CreateMul,
				lexer.DIV:   walker.Info.builder.CreateSDiv,
				lexer.MOD:   walker.Info.builder.CreateSRem,
			}
			var bitops = map[lexer.Kind]func(lhs, rhs llvm.Value, name string) llvm.Value{
				lexer.OR:     walker.Info.builder.CreateOr,
				lexer.AND:    walker.Info.builder.CreateAnd,
				lexer.XOR:    walker.Info.builder.CreateXor,
				lexer.LSHIFT: walker.Info.builder.CreateShl,
				lexer.RSHIFT: walker.Info.builder.CreateAShr,
			}

			var cmps = map[lexer.Kind]struct {
				pred   llvm.IntPredicate
				signed bool
			}{
				lexer.GREAT: {llvm.IntSGT, true},
				lexer.GE:    {llvm.IntSGE, true},
				lexer.LESS:  {llvm.IntSLT, true},
				lexer.LE:    {llvm.IntSLE, true},
				lexer.NE:    {llvm.IntNE, true},
				lexer.EQUAL: {llvm.IntEQ, true},
			}

			switch e.Op {
			case lexer.PLUS, lexer.MINUS, lexer.MUL, lexer.DIV, lexer.MOD:
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)

				var l, r llvm.Value = lhs, rhs
				if l.Type().TypeKind() == llvm.PointerTypeKind {
					l = walker.Info.builder.CreateLoad(l, "")
				}

				if r.Type().TypeKind() == llvm.PointerTypeKind {
					r = walker.Info.builder.CreateLoad(r, "")
				}
				op = ariths[e.Op](l, r, "tmp")

			case lexer.GREAT, lexer.GE, lexer.LESS, lexer.LE, lexer.NE, lexer.EQUAL:
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)

				var l, r llvm.Value = lhs, rhs
				if l.Type().TypeKind() == llvm.PointerTypeKind {
					l = walker.Info.builder.CreateLoad(l, "")
				}

				if r.Type().TypeKind() == llvm.PointerTypeKind {
					r = walker.Info.builder.CreateLoad(r, "")
				}
				op = walker.Info.builder.CreateICmp(cmps[e.Op].pred, l, r, "")

			case lexer.ASSIGN:
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)
				var l, r llvm.Value = lhs, rhs

				if r.Type().TypeKind() == llvm.PointerTypeKind {
					r = walker.Info.builder.CreateLoad(r, "")
				}
				log("ASSIGN: l %s, r %s\n", l.Type(), r.Type())
				op = walker.Info.builder.CreateStore(r, l)

			case lexer.COMMA:
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)
				op = rhs

			case lexer.LOG_OR, lexer.LOG_AND:
				var lhs_bb = walker.Info.builder.GetInsertBlock()
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)

				var val, notval llvm.Value
				if e.Op == lexer.LOG_OR {
					val, notval = llvm.ConstInt(llvm.Int1Type(), 0, false), llvm.ConstInt(llvm.Int1Type(), 1, false)
				} else {
					val, notval = llvm.ConstInt(llvm.Int1Type(), 1, false), llvm.ConstInt(llvm.Int1Type(), 0, false)
				}
				var cmp = walker.Info.builder.CreateICmp(llvm.IntEQ, lhs, val, "")
				var fn = walker.Info.builder.GetInsertBlock().Parent()
				var rhs_bb = llvm.AddBasicBlock(fn, "")
				var end_bb = llvm.AddBasicBlock(fn, "")

				walker.Info.builder.CreateCondBr(cmp, rhs_bb, end_bb)
				walker.Info.builder.SetInsertPoint(rhs_bb, rhs_bb.FirstInstruction())
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)
				walker.Info.builder.CreateBr(end_bb)

				walker.Info.builder.SetInsertPoint(end_bb, end_bb.FirstInstruction())
				var phi = walker.Info.builder.CreatePHI(val.Type(), "")
				//use const `!val` here is more optimal
				phi.AddIncoming([]llvm.Value{notval, rhs}, []llvm.BasicBlock{lhs_bb, rhs_bb})
				op = phi

			case lexer.OR, lexer.XOR, lexer.AND, lexer.LSHIFT, lexer.RSHIFT:
				lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
				rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)

				var l, r llvm.Value = lhs, rhs
				if l.Type().TypeKind() == llvm.PointerTypeKind {
					l = walker.Info.builder.CreateLoad(l, "")
				}

				if r.Type().TypeKind() == llvm.PointerTypeKind {
					r = walker.Info.builder.CreateLoad(r, "")
				}
				op = bitops[e.Op](l, r, "tmp")

			default:
				panic("not implemented")
			}

			ctx.Value = op
			return false
		}
		return true
	}

	walker.WalkDeclRefExpr = func(ws ast.WalkStage, e *ast.DeclRefExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			ctx.Value = Find(e.Name)
		}
		return true
	}

	walker.WalkSizeofExpr = func(ws ast.WalkStage, e *ast.SizeofExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		} else {
		}
		return true
	}
	walker.WalkUnaryOperation = func(ws ast.WalkStage, e *ast.UnaryOperation, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerBubbleUp {
			var val = ctx.Value.(llvm.Value)
			switch e.Op {
			case lexer.TILDE:
				if e.Postfix {
					panic("impossible")
				}
				//CreateNot is bitwise not
				var val2 = walker.Info.builder.CreateLoad(val, val.Name())
				ctx.Value = walker.Info.builder.CreateNot(val2, val.Name())

			case lexer.NOT:
				if e.Postfix {
					panic("impossible")
				}
				var val2 = walker.Info.builder.CreateLoad(val, val.Name())
				var cmp = walker.Info.builder.CreateICmp(llvm.IntNE, val2, llvm.ConstInt(val2.Type(), 0, false), val.Name())
				var val3 = walker.Info.builder.CreateXor(cmp, llvm.ConstInt(llvm.Int1Type(), 1, false), "")
				ctx.Value = walker.Info.builder.CreateZExt(val3, val2.Type(), "")

			case lexer.PLUS:
				if e.Postfix {
					panic("impossible")
				}
				// do nothing

			case lexer.MINUS:
				if e.Postfix {
					panic("impossible")
				}
				ctx.Value = walker.Info.builder.CreateNeg(val, val.Name())

			case lexer.MUL:
				if e.Postfix {
					panic("impossible")
				}
				//pointer deref
				ctx.Value = walker.Info.builder.CreateLoad(val, val.Name())

			case lexer.AND:
				if e.Postfix {
					panic("impossible")
				}
				//address of
				// do nothing, llvm've handled it

			case lexer.INC, lexer.DEC:
				if e.Postfix {
					//TODO: postpone side effect to sequence point?
					var val2 = walker.Info.builder.CreateLoad(val, val.Name())
					var val3 llvm.Value
					if e.Op == lexer.INC {
						val3 = walker.Info.builder.CreateAdd(val2, llvm.ConstInt(val2.Type(), 1, false), "")
					} else {
						val3 = walker.Info.builder.CreateSub(val2, llvm.ConstInt(val2.Type(), 1, false), "")
					}
					walker.Info.builder.CreateStore(val3, val)
					ctx.Value = val2

				} else {
					var val2 = walker.Info.builder.CreateLoad(val, val.Name())
					//side effect
					if e.Op == lexer.INC {
						val2 = walker.Info.builder.CreateAdd(val2, llvm.ConstInt(val2.Type(), 1, false), "")
					} else {
						val2 = walker.Info.builder.CreateSub(val2, llvm.ConstInt(val2.Type(), 1, false), "")
					}
					walker.Info.builder.CreateStore(val2, val)
					ctx.Value = val2
				}
			default:
				panic("not implemented")
			}
		}
		return true
	}
	walker.WalkConditionalOperation = func(ws ast.WalkStage, e *ast.ConditionalOperation, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerBubbleUp {
		}
		return true
	}

	walker.WalkArraySubscriptExpr = func(ws ast.WalkStage, e *ast.ArraySubscriptExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerBubbleUp {
			var sub = ast.WalkAst(e.Sub, walker, ctx).(llvm.Value)
			if sub.Type().TypeKind() == llvm.PointerTypeKind {
				sub = walker.Info.builder.CreateLoad(sub, sub.Name())
			}
			var arr = llvm.ConstInt(llvm.Int32Type(), 0, false)
			var pobj = ast.WalkAst(e.Target, walker, ctx).(llvm.Value)

			ctx.Value = walker.Info.builder.CreateInBoundsGEP(pobj, []llvm.Value{arr, sub}, "")
		}
		return true
	}

	walker.WalkMemberExpr = func(ws ast.WalkStage, e *ast.MemberExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {

		} else {
		}
		return true
	}
	walker.WalkFunctionCall = func(ws ast.WalkStage, e *ast.FunctionCall, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			var params []llvm.Value
			var fn llvm.Value

			fn = walker.Info.Mod.NamedFunction(e.Func.(*ast.DeclRefExpr).Name) //FIXME: not always true
			// so globals are pointers in llvm ir always
			log("WalkFunctionCall %v\n", fn.Type().ElementType())

			if fn.ParamsCount() != len(e.Args) {
				panic("param count mismatch")
			}

			for i, arg := range e.Args {
				var varg = ast.WalkAst(arg, walker, ctx)
				if varg.(llvm.Value).Type() != fn.Param(i).Type() {
					panic(fmt.Sprintf("type mismatch for #%d arg", i))
				}
				log("WalkFunctionCall arg %s\n", varg.(llvm.Value))
				params = append(params, varg.(llvm.Value))
			}

			if fn.Type().ElementType().ReturnType().TypeKind() == llvm.VoidTypeKind {
				// void return should not be named
				ctx.Value = walker.Info.builder.CreateCall(fn, params, "")
			} else {
				ctx.Value = walker.Info.builder.CreateCall(fn, params, "calltmp")
			}
			return false
		}
		return true
	}
	walker.WalkCompoundAssignExpr = func(ws ast.WalkStage, e *ast.CompoundAssignExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			var lhs, rhs, op llvm.Value

			lhs = ast.WalkAst(e.LHS, walker, ctx).(llvm.Value)
			rhs = ast.WalkAst(e.RHS, walker, ctx).(llvm.Value)
			log("WalkCompoundAssignExpr: lhs %v, rhs %v\n", lhs.Type(), rhs.Type())

			switch e.Op {
			case lexer.PLUS_ASSIGN:
				var l, r llvm.Value = lhs, rhs
				if l.Type().TypeKind() == llvm.PointerTypeKind {
					l = walker.Info.builder.CreateLoad(l, lhs.Name())
				}

				if r.Type().TypeKind() == llvm.PointerTypeKind {
					r = walker.Info.builder.CreateLoad(r, rhs.Name())
				}

				var val = walker.Info.builder.CreateAdd(l, r, "")
				walker.Info.builder.CreateStore(val, lhs)
				op = val

			default:
				panic("not implemented")
			}

			ctx.Value = op

			return false
		}
		return true
	}

	walker.WalkCastExpr = func(ws ast.WalkStage, e *ast.CastExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		} else {
		}
		return true
	}
	walker.WalkCompoundLiteralExpr = func(ws ast.WalkStage, e *ast.CompoundLiteralExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		} else {
		}
		return true
	}

	walker.WalkBreakStmt = func(ws ast.WalkStage, e *ast.BreakStmt, ctx *ast.WalkContext) {
		if InSwitchCaseCounting() {
			return
		}
		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(GetBreak())
			}
		}
	}

	walker.WalkContinueStmt = func(ws ast.WalkStage, e *ast.ContinueStmt, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
		}
	}

	walker.WalkInitListExpr = func(ws ast.WalkStage, e *ast.InitListExpr, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
		}
		return true
	}
	walker.WalkFieldDecl = func(ws ast.WalkStage, e *ast.FieldDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
		}
	}
	walker.WalkRecordDecl = func(ws ast.WalkStage, e *ast.RecordDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
		} else {
		}
	}
	walker.WalkEnumeratorDecl = func(ws ast.WalkStage, e *ast.EnumeratorDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
		} else {
		}
	}
	walker.WalkEnumDecl = func(ws ast.WalkStage, e *ast.EnumDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {

		} else {
		}
	}
	walker.WalkTypedefDecl = func(ws ast.WalkStage, e *ast.TypedefDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
			//sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)

		} else {
		}
	}
	walker.WalkVariableDecl = func(ws ast.WalkStage, e *ast.VariableDecl, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}
		if ws == ast.WalkerBubbleUp {
			sym := ctx.Scope.LookupSymbol(e.Sym, ast.OrdinaryNS)
			var vty = symbolTy2llvmType(sym.Type)

			if e.Ctx.Top == ctx.Scope {
				log("decl global %s\n", sym.Name.AsString())
				var val = llvm.AddGlobal(walker.Info.Mod, vty, sym.Name.AsString())
				if e.Init != nil {
					val.SetInitializer(ctx.Value.(llvm.Value))
				}
				ctx.Value = val
				Append(val)
			} else {
				log("decl local %s\n", sym.Name.AsString())
				var v = walker.Info.builder.CreateAlloca(vty, sym.Name.AsString())
				if e.Init != nil {
					walker.Info.builder.CreateStore(ctx.Value.(llvm.Value), v)
				}
				ctx.Value = v
				Append(v)
			}

			return false
		}
		return true
	}
	walker.WalkParamDecl = func(ws ast.WalkStage, e *ast.ParamDecl, ctx *ast.WalkContext) {
		if ws == ast.WalkerPropagate {
		} else {
		}
	}
	walker.WalkFunctionDecl = func(ws ast.WalkStage, e *ast.FunctionDecl, ctx *ast.WalkContext) {
		//FIXME: do one traversal to collect all labels

		sym := ctx.Scope.LookupSymbol(e.Name, ast.OrdinaryNS)
		if ws == ast.WalkerPropagate {
			var ll_fty = symbolTy2llvmType(sym.Type)
			var ll_func = llvm.AddFunction(walker.Info.Mod, sym.Name.AsString(), ll_fty)

			Append(llvm.Value{}) // nil value as delim

			for i, arg := range e.Args {
				//util.Printf("WalkFunctionDecl: arg(%d) %s\n", i, arg.Sym)
				ll_func.Param(i).SetName(arg.Sym)
				Append(ll_func.Param(i))
			}

			var bb = llvm.AddBasicBlock(ll_func, "entry")
			walker.Info.builder.SetInsertPoint(bb, bb.FirstInstruction())
			if len(e.Body.Stmts) == 0 {
				walker.Info.builder.CreateRetVoid()
			}

		} else {
			var fn = walker.Info.Mod.NamedFunction(sym.Name.AsString())
			if fn.LastBasicBlock().LastInstruction().IsNil() {
				// bb has no instrs
				if fn.LastBasicBlock().AsValue().FirstUse().IsNil() {
					fn.LastBasicBlock().EraseFromParent()
				}
			}
			Drop()
			// all labels should be in function scope
			DropAllLabels()
		}
	}
	walker.WalkDeclStmt = func(ws ast.WalkStage, e *ast.DeclStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}
		return true
	}
	walker.WalkExprStmt = func(ws ast.WalkStage, e *ast.ExprStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}
		if ws == ast.WalkerPropagate {
			ast.WalkAst(e.Expr, walker, ctx)
			return false
		}
		return true
	}

	walker.WalkCaseStmt = func(ws ast.WalkStage, e *ast.CaseStmt, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if InSwitchCaseCounting() {
				walker.Info.sw.num_cases++
				return true

			} else {
				var orig = walker.Info.builder.GetInsertBlock()
				var fn = orig.Parent()

				var case_bb = llvm.AddBasicBlock(fn, "")
				var on = ast.WalkAst(e.ConstExpr, walker, ctx).(llvm.Value)
				walker.Info.sw.switch_val.AddCase(on, case_bb)

				walker.Info.sw.cases = append(walker.Info.sw.cases, case_bb)

				// insert a fallthrough for last case
				if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
					walker.Info.builder.CreateBr(case_bb)
				}

				walker.Info.builder.SetInsertPoint(case_bb, case_bb.FirstInstruction())
				if e.Stmt == nil {
					panic("should not be null")
				}
				ast.WalkAst(e.Stmt, walker, ctx)
			}
			return false
		}
		return true
	}
	walker.WalkDefaultStmt = func(ws ast.WalkStage, e *ast.DefaultStmt, ctx *ast.WalkContext) bool {
		if ws == ast.WalkerPropagate {
			if InSwitchCaseCounting() {
				walker.Info.sw.has_default = true
			} else {
				if !walker.Info.sw.has_default {
					panic("impossible")
				}

				var orig = walker.Info.builder.GetInsertBlock()
				walker.Info.sw.default_bb.MoveAfter(orig)
				if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
					walker.Info.builder.CreateBr(walker.Info.sw.default_bb)
				}

				walker.Info.builder.SetInsertPoint(walker.Info.sw.default_bb,
					walker.Info.sw.default_bb.FirstInstruction())
				if e.Stmt == nil {
					panic("should not be null")
				}
				ast.WalkAst(e.Stmt, walker, ctx)
			}
			return false
		}
		return true
	}

	var doConversion = func(val llvm.Value, rty llvm.Type) llvm.Value {
		if rty != val.Type() {
			if val.Type().TypeKind() == llvm.IntegerTypeKind && rty.TypeKind() == llvm.IntegerTypeKind {
				var w1, w2 = val.Type().IntTypeWidth(), rty.IntTypeWidth()
				switch {
				case w1 < w2:
					val = walker.Info.builder.CreateZExt(val, rty, "")
				case w1 > w2:
					val = walker.Info.builder.CreateTrunc(val, rty, "")
				}
				return val
			}

		}

		//FIXME: this is not always correct
		if val.Type().TypeKind() == llvm.PointerTypeKind {
			return walker.Info.builder.CreateLoad(val, "")
		}

		return val
	}
	walker.WalkReturnStmt = func(ws ast.WalkStage, e *ast.ReturnStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}
		if ws == ast.WalkerBubbleUp {
			var fn = walker.Info.builder.GetInsertBlock().Parent()
			var val = ctx.Value.(llvm.Value)

			var rty = fn.Type().ElementType().ReturnType()
			val = doConversion(val, rty)

			//TODO: collect rets from all paths, and do one ret at the end of function
			ctx.Value = walker.Info.builder.CreateRet(val)
		}
		return true
	}
	walker.WalkSwitchStmt = func(ws ast.WalkStage, e *ast.SwitchStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return false
		}

		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			PushSwitchState()

			walker.Info.sw.count_cases = true
			log("SwitchStmt collecting  cases\n")
			ast.WalkAst(e.Body, walker, ctx)
			walker.Info.sw.count_cases = false

			log("SwitchStmt collect #%d cases, default %d\n", walker.Info.sw.num_cases,
				walker.Info.sw.has_default)

			var cond_bb = llvm.AddBasicBlock(fn, "")
			var default_bb llvm.BasicBlock
			var end_bb = llvm.AddBasicBlock(fn, "")

			if walker.Info.sw.has_default {
				default_bb = llvm.AddBasicBlock(fn, "default")
			}
			walker.Info.sw.default_bb = default_bb
			PushBreak(end_bb)

			walker.Info.builder.CreateBr(cond_bb)

			walker.Info.builder.SetInsertPoint(cond_bb, cond_bb.FirstInstruction())
			var cond = ast.WalkAst(e.Cond, walker, ctx).(llvm.Value)

			for cond.Type().TypeKind() == llvm.PointerTypeKind {
				cond = walker.Info.builder.CreateLoad(cond, "")
			}
			if default_bb.IsNil() {
				walker.Info.sw.switch_val = walker.Info.builder.CreateSwitch(cond, end_bb,
					walker.Info.sw.num_cases)
			} else {
				walker.Info.sw.switch_val = walker.Info.builder.CreateSwitch(cond, default_bb,
					walker.Info.sw.num_cases)
			}

			ast.WalkAst(e.Body, walker, ctx)

			orig = walker.Info.builder.GetInsertBlock()
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(GetBreak())
			}

			end_bb.MoveAfter(fn.LastBasicBlock())
			walker.Info.builder.SetInsertPoint(end_bb, end_bb.LastInstruction())
			PopBreak()

			PopSwitchState()
			return false
		}
		return true
	}
	walker.WalkWhileStmt = func(ws ast.WalkStage, e *ast.WhileStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return true
		}
		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			var cond_bb = llvm.AddBasicBlock(fn, "")
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(cond_bb)
			}
			walker.Info.builder.SetInsertPoint(cond_bb, cond_bb.FirstInstruction())
			var cond = ast.WalkAst(e.Cond, walker, ctx).(llvm.Value)

			var body_bb = llvm.AddBasicBlock(fn, "")
			var merge_bb = llvm.AddBasicBlock(fn, "")
			PushBreak(merge_bb)
			walker.Info.builder.CreateCondBr(cond, body_bb, merge_bb)

			walker.Info.builder.SetInsertPoint(body_bb, body_bb.FirstInstruction())
			if e.Body != nil {
				ast.WalkAst(e.Body, walker, ctx)
			}

			orig = walker.Info.builder.GetInsertBlock()
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(cond_bb)
			}

			PopBreak()
			walker.Info.builder.SetInsertPoint(merge_bb, merge_bb.LastInstruction())
			return false
		}
		return true
	}
	walker.WalkDoStmt = func(ws ast.WalkStage, e *ast.DoStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return true
		}
		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			// cond_bb should create before body travesal, so break stmt inside body can use this
			// as jump-out label
			var cond_bb = llvm.AddBasicBlock(fn, "")
			PushBreak(cond_bb)
			var body_bb = llvm.AddBasicBlock(fn, "do")
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(body_bb)
			}
			walker.Info.builder.SetInsertPoint(body_bb, body_bb.FirstInstruction())
			if e.Body != nil {
				ast.WalkAst(e.Body, walker, ctx)
			}

			orig = walker.Info.builder.GetInsertBlock()
			cond_bb.MoveAfter(orig)
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(cond_bb)
			}
			walker.Info.builder.SetInsertPoint(cond_bb, cond_bb.FirstInstruction())
			var cond = ast.WalkAst(e.Cond, walker, ctx).(llvm.Value)

			var merge_bb = llvm.AddBasicBlock(fn, "")
			walker.Info.builder.CreateCondBr(cond, body_bb, merge_bb)

			PopBreak()
			walker.Info.builder.SetInsertPoint(merge_bb, merge_bb.LastInstruction())
			return false
		}

		return true
	}
	walker.WalkIfStmt = func(ws ast.WalkStage, e *ast.IfStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return true
		}
		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			var then_bb = llvm.AddBasicBlock(fn, "")
			var else_bb = llvm.AddBasicBlock(fn, "")
			var merge_bb = llvm.AddBasicBlock(fn, "")

			var cond = ast.WalkAst(e.Cond, walker, ctx).(llvm.Value)
			walker.Info.builder.CreateCondBr(cond, then_bb, else_bb)

			walker.Info.builder.SetInsertPoint(then_bb, then_bb.FirstInstruction())
			if e.TrueBranch != nil {
				ast.WalkAst(e.TrueBranch, walker, ctx)
			}
			if then_bb.LastInstruction().IsNil() || then_bb.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(merge_bb)
			}

			walker.Info.builder.SetInsertPoint(else_bb, else_bb.FirstInstruction())
			if e.FalseBranch != nil {
				ast.WalkAst(e.FalseBranch, walker, ctx)
			}

			orig = walker.Info.builder.GetInsertBlock()
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(merge_bb)
			}

			walker.Info.builder.SetInsertPoint(merge_bb, merge_bb.LastInstruction())
			return false
		}
		return true
	}

	walker.WalkLabelStmt = func(ws ast.WalkStage, e *ast.LabelStmt, ctx *ast.WalkContext) {
		if InSwitchCaseCounting() {
			return
		}

		if ws == ast.WalkerPropagate {
			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			var label_bb = FindLabel(e.Label)
			if label_bb.IsNil() {
				label_bb = llvm.AddBasicBlock(fn, e.Label)
				AddLabel(e.Label, label_bb)
			} else {
				label_bb.MoveAfter(orig)
			}
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(label_bb)
			}

			walker.Info.builder.SetInsertPoint(label_bb, label_bb.FirstInstruction())
		}
	}
	walker.WalkGotoStmt = func(ws ast.WalkStage, e *ast.GotoStmt, ctx *ast.WalkContext) {
		if InSwitchCaseCounting() {
			return
		}
		if ws == ast.WalkerPropagate {
			var label_bb = FindLabel(e.Label)
			if label_bb.IsNil() {
				// this is forward jump
				var fn = walker.Info.builder.GetInsertBlock().Parent()
				label_bb = llvm.AddBasicBlock(fn, e.Label)
				AddLabel(e.Label, label_bb)
			}
			walker.Info.builder.CreateBr(label_bb)
		}
	}

	walker.WalkForStmt = func(ws ast.WalkStage, e *ast.ForStmt, ctx *ast.WalkContext) bool {
		if InSwitchCaseCounting() {
			return true
		}
		if ws == ast.WalkerPropagate {
			Append(llvm.Value{}) // nil value as delim

			var orig = walker.Info.builder.GetInsertBlock()
			var fn = orig.Parent()

			var cond_bb = llvm.AddBasicBlock(fn, "")
			var body_bb = llvm.AddBasicBlock(fn, "")
			var step_bb = llvm.AddBasicBlock(fn, "")
			var end_bb = llvm.AddBasicBlock(fn, "")

			if e.Decl != nil {
				ast.WalkAst(e.Decl, walker, ctx)
			} else if e.Init != nil {
				ast.WalkAst(e.Init, walker, ctx)
			}
			walker.Info.builder.CreateBr(cond_bb)

			walker.Info.builder.SetInsertPoint(cond_bb, cond_bb.FirstInstruction())
			var cond = ast.WalkAst(e.Cond, walker, ctx).(llvm.Value)
			walker.Info.builder.CreateCondBr(cond, body_bb, end_bb)

			walker.Info.builder.SetInsertPoint(body_bb, body_bb.FirstInstruction())
			if e.Body != nil {
				ast.WalkAst(e.Body, walker, ctx)
			}
			orig = walker.Info.builder.GetInsertBlock()
			if orig.LastInstruction().IsNil() || orig.LastInstruction().IsATerminatorInst().IsNil() {
				walker.Info.builder.CreateBr(step_bb)
			}

			walker.Info.builder.SetInsertPoint(step_bb, step_bb.FirstInstruction())
			if e.Step != nil {
				ast.WalkAst(e.Step, walker, ctx)
			}
			walker.Info.builder.CreateBr(cond_bb)

			walker.Info.builder.SetInsertPoint(end_bb, end_bb.LastInstruction())

			Drop()
			return false
		}
		return true
	}
	walker.WalkCompoundStmt = func(ws ast.WalkStage, e *ast.CompoundStmt, ctx *ast.WalkContext) {
		if InSwitchCaseCounting() {
			return
		}
		if ws == ast.WalkerPropagate {
			Append(llvm.Value{}) // nil value as delim
		} else {
			Drop()
		}
	}

	return walker
}
