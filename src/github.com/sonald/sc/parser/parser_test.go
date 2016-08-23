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

func TestParseSimpleDecls(t *testing.T) {
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
short int si;

// these are errors, but better report warnings and proceed
int int int iii;
long long long lll;
short long sl;
char long cl;
short int long il;
unsigned int signed ius; // this is error, shoud be panic
char int ci;

void* fp = 0;

unsigned const char volatile uccv;

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

extern int logE(void);

void signal(int signo, ...);
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.funcDecls == nil || len(tu.funcDecls) != 3 {
			t.Errorf("failed to parse func decl")
		}

		if tu.recordDecls == nil || len(tu.recordDecls) != 3 {
			t.Errorf("failed to parse some records")
		}

		if tu.varDecls == nil || len(tu.varDecls) != 15 {
			t.Errorf("failed to parse some vars")
		}
	}
}

func TestParseComplexDecls(t *testing.T) {
	var text = `
int (*ids)[5];

// this is acctually semantically wrong
char alphas[(*ids)[2]][(*ids)[1]];

int (*fp)(void, void);
int *const *const ccfp[];
int (*const wokers [])(unsigned int);
// an array of grid (each grid is a two-dim array)
int (grids[2])[5];
int* (grids2[2])[5][4];
int (*const* grids3[2])[5];

char **(*(*(*x)[100])(int, char *, double *const**, void (*)(int **, char [])))[50];
const unsigned char volatile (
	*const(
		*volatile(*volatile const A)(
			unsigned int (*const)(const char, int, float), char *const*)
		)[12]
	)(int (**)[50]);

struct work {
	int kind;
	int payload[10];
} s;
char alphas2[alphas[0] + alphas[1]][fp()][sizeof s.payload + s.kind];
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.varDecls == nil || len(tu.varDecls) != 12 {
			t.Errorf("failed to parse some vars")
		}
	}
}

func TestParseUserTypes(t *testing.T) {
	var text = `
struct Tree {
	int val;
};

struct Tree;
struct Tree n1;

typedef struct Tree tree_t;
tree_t n2;

int typedef unsigned iu_t;

typedef int size_t;
size_t sz = 2;

int unsigned typedef * const *grid_t[2];
grid_t grid;

int typedef (*designator)(int ,int); 

int typedef unsigned * *const ui_t[2], (*ui_t2)(void);

enum Color;
enum Color clr;
enum Color {red, green = 3, blue};

typedef enum Color color_t;
enum Color clr2 = green;
color_t clr3 = red;
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if tu.typedefDecls == nil || len(tu.typedefDecls) != 8 {
			t.Errorf("failed to parse some typedefs")
		}
		if tu.varDecls == nil || len(tu.varDecls) != 7 {
			t.Errorf("failed to parse some vars")
		}
		if tu.enumDecls == nil || len(tu.enumDecls) != 2 {
			t.Errorf("failed to parse some vars")
		}
	}
}

func testParseNamespaces(t *testing.T) {
	var text = `
// c has multiple parallel namespaces, the same name may 
// have another meaning in different namespaces.
struct Tree {
	int val;
};

typedef struct Tree Tree;
int typedef unsigned compound[2];
struct compound {
	int first, second;
} compound;

//enum Color {red, green, blue};
//struct red {
//};
`
	ast := testTemplate(t, text)
	if _, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	}
}

func testParseTypeCasts(t *testing.T) {
	var text = `
(int (*fp)[5])a;
(int * id[3])a;
(int (*id)[4])a;
(int (*)[*])a;
(int *())a;
(int (*)(void))a;
`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		//if tu.funcDecls == nil || len(tu.funcDecls) != 1 {
		//t.Errorf("failed to parse func decl")
		//}

		//if tu.recordDecls == nil || len(tu.recordDecls) != 3 {
		//t.Errorf("failed to parse some records")
		//}

		if tu.varDecls == nil || len(tu.varDecls) != 1 {
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

	sizeof c;
	a/sizeof c+2;
	int sz = sizeof (struct grid {int val;});
}
	`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if len(tu.funcDecls) != 1 {
			t.Errorf("failed to parse func decl")
		}

		fd := tu.funcDecls[0]
		if len(fd.Args) != 2 {
			t.Errorf("# of arguments = %d, expect 2", len(fd.Args))
		}

		if len(fd.Body.Stmts) != 20 {
			t.Errorf("# of statements = %d, expect 20", len(fd.Body.Stmts))
		}

	}
}

func TestParseComplexExprs(t *testing.T) {
	var text = `
int foo()
{
	struct Tree {
		int val;
		struct Tree* left, *right;
	};

	struct Tree t = (struct Tree) {10, NULL, NULL};
}
	`
	ast := testTemplate(t, text)
	if tu, ok := ast.(*TranslationUnit); !ok {
		t.Errorf("parse failed")
	} else {
		if len(tu.funcDecls) != 1 {
			t.Errorf("failed to parse func decl")
		}

		fd := tu.funcDecls[0]

		if len(fd.Body.Stmts) != 2 {
			t.Errorf("# of statements = %d, expect 2", len(fd.Body.Stmts))
		}

	}
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
