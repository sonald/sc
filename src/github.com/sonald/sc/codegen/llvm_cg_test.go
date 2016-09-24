package codegen

import (
	"flag"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/parser"
	"github.com/sonald/sc/sema"
	"llvm.org/llvm/bindings/go/llvm"
	"os"
	"strings"
	"testing"
	"unsafe"
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
	if len(os.Getenv("DEBUG")) > 0 {
		p.DumpAst()
	}
	sema.DumpReports()

	if len(sema.Reports) > 0 {
		return nil
	}

	if top == nil {
		t.Errorf("parse failed")
	} else {
		mod := ast.WalkAst(top, MakeLLVMCodeGen()).(llvm.Module)
		llvm.VerifyModule(mod, llvm.AbortProcessAction)

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
	return **p2;
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
	const expect = 41
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
	const expect = 41
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

func TestSimple9(t *testing.T) {
	// test break
	var text = `
int foo() {
	int i = 2, j = 3;
	while (j-- > 0) {
		switch (i) {
			case 1: 
				case 2: break;
			case 3:
				default: 
					break;
		}

	}

	return j;
}

int bar() {
	int i = 3, j = 3;
	switch (i) {
		case 1: 
			case 2:
				while (j-- > 0) {
					break;
					case 3:
						default: 
							break;
				}

	}

	return j;
}

int main(int argc)
{
	if (argc == 0) 
		return foo();
	else
		return bar();

    return 0;
}

`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		for i := 0; i < 2; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			var expect = -1
			if i == 1 {
				expect = 3
			}
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expect) {
				t.Errorf("wrong answer for arg %d: expect %d, ret %d", i, expect, ret.Int(true))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple10(t *testing.T) {
	//test nested switch statements
	var text = `
int foo() {
    int i = 1, j = 4, k = 4;
    switch (i) {
        case 1: 
            k--;
        case 2:
            k--;
            switch (j) {
                case 3:
                    k--;
                case 4:
                    k--;
                default: 
                    break;
            }
        default: 
            break;
    }
    return k;
}

int main()
{
    return foo();
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var expect = 1
		ret := engine.RunFunction(mod.NamedFunction("main"), nil)
		if ret.Int(true) != uint64(expect) {
			t.Errorf("wrong answer: expect %d, ret %d", expect, ret.Int(true))
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple11(t *testing.T) {
	//test goto's
	var text = `
int foo(int n) {
	int k = 0;
    int l = 1;
	switch(n) {
	case 1: {
		k++;
		goto _half;
	}
	case 2: k++;
_half:
	case 3: k++;
	case 4: {
		k++;
        if (l-- < 0) {
            break;
        }
        goto _half;
	}
	}

    return k;
}

int main(int arg)
{
    return foo(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var expects = []int{7, 7, 6, 5}
		for i := 1; i < 5; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expects[i-1]) {
				t.Errorf("wrong answer: expect %d, ret %d", expects[i-1], ret.Int(true))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple12(t *testing.T) {
	var text = `
int f1() {
	int k = 42;
	int *p = &k;
	return !*p;
}

int f2() {
	int a = 5, b = 1;
	return a > 4 && ++b >= 2;
}

int f3() {
	int a = 5;
	return  ++a * 20 / 21 - 30 + 40 % 17;
}

int f4() {
	int a = -2, b = 10;
	return (a << 1) + (b >> 1) ;
}

int f5() {
	int a = -2, b = 10;
	return b ^ (a&0xf0) + (b|0x0f);
}

int foo(int n) {
	int k = 42;
	switch (n) {
	case 1: 
		k = f1();
		break;
	case 2:
		k = f2();
		break;
	case 3:
		k = f3();
		break;
	case 4:
		k = f4();
		break;
	case 5:
		k = f5();
		break;
	default:
		k = !k;
	}

/*
    a--+-b++;
    kernel[2] + a;
    add(a+b, !b++);
    st->st_time - ~1;
    "string";
    a.bar(a,b);
    a>>1;
    b += a<<1;
    a ^ (b | 0xee) & 0xff;
    a>1?  ++a : b-- + 4;
    (float)a + 2.0;
    (float)kernel[2] / 2; 
	int c = b[3] + a[2];

	sizeof c;
	a/sizeof c+2;
	int sz = sizeof (struct grid {int val;});
*/
	return k;
}

int main(int arg)
{
    return foo(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var expects = []int{0, 1, -19, 1, 245}
		for i := 1; i < 6; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expects[i-1]) {
				t.Errorf("wrong answer for %d: expect %d, ret %d", i, expects[i-1], int(ret.Int(true)))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple13(t *testing.T) {
	var text = `

int foo(int n) {
    int a[5];
    for (int i = 0; i < 5; i++) {
        a[i] = i*i;
    }
	return a[n];
}

int main(int arg)
{
    return foo(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var expects = []int{0, 1, 4, 9, 16}
		for i := 0; i < 5; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expects[i]) {
				t.Errorf("wrong answer for %d: expect %d, ret %d", i, expects[i-1], int(ret.Int(true)))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}

func TestSimple14(t *testing.T) {
	var text = `

int foo(int n) {
    int a[5][6];
    for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 6; j++) {
			a[i][j] = i*j;
		}
    }

    int k, ret = 0;
    for (k = 0; k < 5; k++) {
        ret += a[k][n] + a[n][k];
    }
	return ret;
}

int main(int arg)
{
    return foo(arg);
}
`
	var run = func(mod llvm.Module, engine llvm.ExecutionEngine) {
		var expects = []int{0, 20, 40, 60, 80}
		for i := 0; i < 5; i++ {
			var args = []llvm.GenericValue{
				llvm.NewGenericValueFromInt(llvm.Int32Type(), uint64(i), false),
			}
			ret := engine.RunFunction(mod.NamedFunction("main"), args)
			if ret.Int(true) != uint64(expects[i]) {
				t.Errorf("wrong answer for %d: expect %d, ret %d", i, expects[i-1], int(ret.Int(true)))
			}
		}

	}
	testTemplate(t, text, nil, 0, run)
}
func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
