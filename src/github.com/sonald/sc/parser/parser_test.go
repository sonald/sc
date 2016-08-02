package parser

import (
	"flag"
	"os"
	"strings"
	"testing"
)

func testTemplate(t *testing.T, text string) Ast {
	opts := ParseOption{
		Filename: "./test.txt",
		Verbose:  true,
	}

	opts.Reader = strings.NewReader(text)
	p := NewParser()
	ast := p.Parse(&opts)

	p.DumpAst()

	return ast
}

func TestParseScope(t *testing.T) {
}

func TestParseDecls(t *testing.T) {
	var text = `
static const int *id, *id2;
register unsigned int global_var;
double d;
char unsigned ch = 2;
signed int signed iss; // this is ok, but should be warned
signed long long int lli;
int long il;
int signed c;
void* fp;
int int int iii;
short int si;
long long long lll;

short long sl;
char long cl;
short int long il;
unsigned int signed ius; // this is error, shoud be panic
char int ci;

void* fp = 0;


// only one dimension supported now
float kernel[10];

// this is complex
struct Grid {
    int : 2;
    int flag: 5;
    struct sub {
		float radius;
    } sub;
} grid;

// Type Tree and var Tree can be distinguished by parser
struct Tree {
    int payload;
    struct Tree * Left, *Right;
} Tree;

static const int add(const int *a, const int b);
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.funcDecls == nil || len(tu.funcDecls) != 1 {
			t.Errorf("failed to parse func decl")
		}

		if tu.recordDecls == nil || len(tu.recordDecls) != 3 {
			t.Errorf("failed to parse some records")
		}

		if tu.varDecls == nil || len(tu.varDecls) != 17 {
			t.Errorf("failed to parse some vars")
		}
	}
}

func testRecordDesignator(t *testing.T) {
	var text = `
struct grid {
	int flags: 10;
	int val[2][2];
} g = {
	.flags = 0x12,
};

`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.varDecls == nil || len(tu.varDecls) != 1 {
			t.Errorf("failed to parse some arrays")
		}
	}
}

func TestForwardDecl(t *testing.T) {
	var text = `
struct grid;
int mul(struct grid* g1, struct grid* g2)
{
}

struct grid {
	int val[2][2];
};

`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.recordDecls == nil || len(tu.recordDecls) != 2 {
			t.Errorf("failed to parse some records")
		}
	}
}

func TestParseArray(t *testing.T) {
	var text = `
float grid[10][10];
int box[][2];
int N = 10;
// int arr[N];
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.varDecls == nil || len(tu.varDecls) != 3 {
			t.Errorf("failed to parse some arrays")
		}
	}
}

//FIXME: this is be done by sema
func testDetectLoop(t *testing.T) {
	var text = `
struct Tree {
    int payload;
    struct Tree Left, *Right;
} tree;

/*
struct Node {
    int payload;
    const struct Node Left, *Right;
} nd;

// indirect nest
struct Node {
    int payload;
    struct Node2 child;
} nd;
struct Node2 {
	struct Node val;
};
*/
`
	testTemplate(t, text)
}

func TestParseIf(t *testing.T) {
	var text = `
int main(int arg)
{
	if (arg > 0) {
		1;
	} else {
		0;
	}

	if (arg++) 
		--arg;
	else
		if (arg > 4) 
		   2;
		else
			arg += 1;
}
`
	testTemplate(t, text)
}

func TestParseWrongFor(t *testing.T) {
	var text = `
int main(int arg)
{
	int total = 0;
	//ERROR: struct is not allowed here
	for (struct Grid {int sz;} g = {10}; g.sz < argc; g.sz++) {
		total += g.sz;
	}
}
`
	testTemplate(t, text)
}

func TestParseIterate(t *testing.T) {
	var text = `
int main(int arg)
{
	int total = 0;
	for (int i = 0; i < arg; i++) {
		total += i;
	}

	while (arg > 0) 
		--arg;

	do --arg; while (arg > 0);

	goto _done;
	switch (arg) {
	case 1: return 100;
	case -1: return -100;
	default: return 0;
	}

_done:
	return arg;
}
`
	testTemplate(t, text)
}

func TestParseIllegalStmt(t *testing.T) {
	var text = `
int main(int arg)
{
	int total = 0;
	for (int i = 0; i < arg; i++) {
		total += i
	}

	while (arg > 0) 
		--arg;

	do --arg while (arg > 0);

	if (arg > 0) 
		return 2
	else
		return 3
}
`
	testTemplate(t, text)
}

func TestParseExpr(t *testing.T) {
	var text = `
// this is all expressions we support currently
int foo(int a, int b)
{
	// this is only syntactically legal
    int a = {1,{2,3}};
    int b[] = {1,2,3,};
    ++a * 20 / 21 - 30 + 40 % 17;
    a--+-b++;
    kernel[2] + a;
    add(a+b, !b++);
    st->st_time - ~1;
    "string";
    a.bar(a,b);
    a>>1;
    b += a<<1;
    a&0xf0 + b&0x0f;
    a ^ (b | 0xee) & 0xff;
    a>1?  ++a : b-- + 4;
    (float)a + 2.0;
    (float)kernel[2] / 2; 
	int c = b[3] + a[2];
}
	`
	testTemplate(t, text)
}

func TestParseIllegalExpr(t *testing.T) {
	var text = `
int foo(int a, int b)
{
	int a, b;
	a++b+2;
	do a++ while (b > 0);
    add(a++b, !b++);
    a.bar(a,);
    a ^ (b | ) & 0xff;
	int c = b[3]  a[2];
}
	`
	testTemplate(t, text)
}

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
