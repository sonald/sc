package codegen

import (
	"flag"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/parser"
	"github.com/sonald/sc/sema"
	"unsafe"
	//"github.com/sonald/sc/util"
	"llvm.org/llvm/bindings/go/llvm"
	"os"
	"strings"
	"testing"
)

func testTemplate(t *testing.T, text string, args []llvm.GenericValue, expect uint64) ast.Ast {
	opts := parser.ParseOption{
		Filename: "./test.txt",
		Verbose:  true,
	}

	opts.Reader = strings.NewReader(text)
	p := parser.NewParser()
	top := p.Parse(&opts)

	sema.RunWalkers(top)
	p.DumpAst()
	sema.DumpReports()

	if len(sema.Reports) > 0 {
		return nil
	}

	if top == nil {
		t.Errorf("parse failed")
	} else {
		mod := ast.WalkAst(top, MakeLLVMCodeGen()).(llvm.Module)
		llvm.VerifyModule(mod, llvm.PrintMessageAction)

		if engine, err := llvm.NewExecutionEngine(mod); err == nil {
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != expect {
				t.Errorf("wrong answer, expect %d, ret %d", expect, ret.Int(true))
			}
		} else {
			t.Errorf(err.Error())
		}
	}
	return top
}

func TestSimple1(t *testing.T) {
	var text = `
int main(int argc, char *argv[]) {
	return 42;
}
`
	var args = []llvm.GenericValue{
		llvm.NewGenericValueFromInt(llvm.Int32Type(), 0, true),
		llvm.NewGenericValueFromPointer(unsafe.Pointer(uintptr(0))),
	}
	const expect = 42
	testTemplate(t, text, args, expect)
}

func TestSimple2(t *testing.T) {
	var text = `
static int debug = 0;

int add(int a, int b) {
	return a+b;
}

static int trace() {
	return debug * 10;
}

int main(int argc, char *argv[]) {
	if (debug > 0) {
		int a = trace();

		return a * a;
	}

	return add(18, 24);
}
`
	var args = []llvm.GenericValue{
		llvm.NewGenericValueFromInt(llvm.Int32Type(), 0, true),
		llvm.NewGenericValueFromPointer(unsafe.Pointer(uintptr(0))),
	}

	const expect = 42
	testTemplate(t, text, args, expect)
}

func TestSimple3(t *testing.T) {
	var text = `
static int debug = 42;

int main() {
	if (debug > 0) {
		int a = 0;
	} else {
		int a = 1;
	}

	int *p = &debug;
	int **p2 = &p;
	return **p;
}
`
	testTemplate(t, text, nil, 42)
}

func TestSimple4(t *testing.T) {
	var text = `
int main() {
	int total = -3;
	int N = 10;
	for (int i = 0; i < N; i++) {
		total += i;
	}

	int ret = total+++-1;
	return ret;
}
`
	const expect = 42
	testTemplate(t, text, nil, expect)
}

func TestSimple5(t *testing.T) {
	var text = `
int main() {
	int total = -3;
	int N = 10, i = 0;
	while (i < N) {
		total += i++;
	}

	total = -3;
	i = 0;
	do {
		total += i++;
	} while (i < N);

	int ret = total+++-1;
	return ret;
}
`
	const expect = 42
	testTemplate(t, text, nil, expect)
}

func TestSimple6(t *testing.T) {
	var text = `
int fib(int n) {
	if (n <= 1) return 1;
	else return fib(n-1) + fib(n-2);
}

int main() {
	return fib(9);
}
`
	const expect = 55
	testTemplate(t, text, nil, expect)
}

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
