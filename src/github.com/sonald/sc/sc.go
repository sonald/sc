package main

import (
	"flag"
	"fmt"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/codegen"
	"github.com/sonald/sc/parser"
	"github.com/sonald/sc/sema"
	"llvm.org/llvm/bindings/go/llvm"
	"os"
	"path"
)

var (
	beVerbose  bool = false
	dumpTokens bool = false
	dumpAst    bool = false
	justRun    bool = false
)

func setupFlags() {
	flag.BoolVar(&beVerbose, "verbose", beVerbose, "increase debug output")
	flag.BoolVar(&dumpTokens, "dump-tokens", dumpTokens, "dump tokens scanned")
	flag.BoolVar(&dumpAst, "dump-ast", dumpAst, "dump ast parsed")
	flag.BoolVar(&justRun, "run", justRun, "run code")
}

func parse(opts *parser.ParseOption) bool {
	p := parser.NewParser()
	var tu = p.Parse(opts)

	sema.RunWalkers(tu)
	sema.DumpReports()
	if dumpAst {
		p.DumpAst()
	}

	if len(sema.Reports) > 0 {
		return false
	}

	if tu == nil {
		return false
	}

	mod := ast.WalkAst(tu, codegen.MakeLLVMCodeGen()).(llvm.Module)
	llvm.VerifyModule(mod, llvm.PrintMessageAction)

	if justRun {
		if engine, err := llvm.NewExecutionEngine(mod); err == nil {
			ret := engine.RunFunction(mod.NamedFunction("main"), nil)
			fmt.Print(ret.Int(true))
		} else {
			fmt.Fprintf(os.Stderr, err.Error())
			return false
		}
		return true
	}

	llvm.InitializeAllTargetInfos()
	llvm.InitializeAllTargets()
	llvm.InitializeAllTargetMCs()
	llvm.InitializeAllAsmParsers()
	llvm.InitializeNativeAsmPrinter()

	target, err := llvm.GetTargetFromTriple(llvm.DefaultTargetTriple())
	if err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return false
	}
	var machine = target.CreateTargetMachine(llvm.DefaultTargetTriple(), "generic", "",
		llvm.CodeGenLevelNone, llvm.RelocDefault, llvm.CodeModelDefault)

	var td = machine.TargetData()
	mod.SetDataLayout(td.String())
	mod.SetTarget(target.Name())
	defer machine.Dispose()

	buf, err := machine.EmitToMemoryBuffer(mod, llvm.ObjectFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return false
	}

	file, err := os.Create(path.Base(opts.Filename) + ".o")
	if err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return false
	}

	_, err = file.Write(buf.Bytes())
	if err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return false
	}
	file.Close()

	return true
}

func main() {
	setupFlags()
	flag.Parse()

	if flag.NArg() == 0 {
		opts := parser.ParseOption{
			Verbose: beVerbose,
		}

		opts.Reader = os.Stdin
		if !parse(&opts) {
			return
		}
	}

	for _, f := range flag.Args() {
		opts := parser.ParseOption{
			Filename: f,
			Verbose:  beVerbose,
		}

		if r, err := os.Open(f); err == nil {
			opts.Reader = r
			if !parse(&opts) {
				return
			}

		} else {
			fmt.Errorf("%s", err)
		}
	}

}
