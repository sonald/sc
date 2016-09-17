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

func testTemplate(t *testing.T, text string, args []llvm.GenericValue, expect uint64,
	run func(llvm.Module, llvm.ExecutionEngine)) ast.Ast {
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
			if run != nil {
				run(mod, engine)
			} else {
				ret := engine.RunFunction(mod.NamedFunction("main"), args)
				if ret.Int(true) != expect {
					t.Errorf("wrong answer, expect %d, ret %d", expect, ret.Int(true))
				}
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
	testTemplate(t, text, args, expect, nil)
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
	testTemplate(t, text, args, expect, nil)
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
	testTemplate(t, text, nil, 42, nil)
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
	testTemplate(t, text, nil, expect, nil)
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
	testTemplate(t, text, nil, expect, nil)
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
	testTemplate(t, text, nil, expect, nil)
}

func TestSimple7(t *testing.T) {
	var text = `
int foo(int n) {
	switch (n) {
	case 0: 
		case 1:
			return 1;
	break;
	default:
		return foo(n-1) + foo(n-2);
	break;
	}
}

int baz(int n) {
    int ret = 0;
	switch (n) {
        case 0: 
            case 1: {
				ret--;
                case 2: 
                    ++ret; 
			}
        ret++;
		break;
        case 3: {
            ret = 3;
			break;
            case 4:
                ret = 5;
        }
        break; 
        default:
            return baz(n-1) + baz(n-2);
        break;
	}
    return ret;
}

int main(int arg) {
	return baz(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var fib func(n int) int
		fib = func(n int) int {
			switch n {
			case 0, 1:
				return 1
			default:
				return fib(n-1) + fib(n-2)
			}
		}

		for i := 0; i < 10; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			var expect = fib(i)
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expect) {
				t.Errorf("wrong answer for arg %d: expect %d, ret %d", i, expect, ret.Int(true))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple8(t *testing.T) {
	var text = `
int bar(int n) {
	int ret = 0;
	int r = (n+3) / 4;
	switch (n % 4) {
	case 0: do { ret++;
	case 3: ret++;
	case 2: ret++;
	case 1: ret++;
	} while (--r > 0);
	}

	return ret;
}

int main(int arg) {
	return bar(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		for i := 1; i < 20; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			var expect = i
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expect) {
				t.Errorf("wrong answer for arg %d: expect %d, ret %d", i, expect, ret.Int(true))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}
func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
