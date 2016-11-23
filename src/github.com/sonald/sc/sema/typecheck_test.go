package sema

import (
	"flag"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/parser"
	"os"
	"strings"
	"testing"
)

func testTemplate(t *testing.T, text string) (ast.Ast, *parser.Parser) {
	opts := parser.ParseOption{
		Filename: "./test.txt",
		Verbose:  true,
	}

	opts.Reader = strings.NewReader(text)
	p := parser.NewParser()
	top := p.Parse(&opts)

	Reports = nil
	p.DumpAst()
	return top, p
}

func TestSimpleDecls(t *testing.T) {
	var text = `
int a = b + 2;

int b = 4;

ssize_t fread(void *restrict ptr, size_t size, size_t nitems, FILE *restrict stream);

int main(int argc, char *argv[]) {
	int buf[BUF_LEN];
	FILE* fp = fopen("test.text", "r");
	if (!fp) {
		return -1;
	}

	fread((void*)bufp, sizeof buf, 1, fp);
	fclose(fp);
}
`
	top, _ := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeReferenceResolve())
		DumpReports()
		if len(Reports) != 8 {
			t.Errorf("should have 8 reports")
		}
	}
}

func TestRefs(t *testing.T) {
	var text = `
int main() {
	int total = 0;
	int N = 10;
	for (int i = 0; i < N; i++) {
		total += i;
	}

	return total2;
}
`
	top, _ := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeReferenceResolve())
		DumpReports()
		if len(Reports) != 1 {
			t.Errorf("should have 1 reports")
		}
	}
}

func TestRefs2(t *testing.T) {
	var text = `
struct Node {
	int val;
	struct Node *left, *right;
};

int foo(int n) {
	struct Node n1;
	n1.val = n;
	n2.left = n1.right = 0;
	n1.payload = 2;
	return n1.val;
}

int bar(int n) {
	struct Node n1;
	struct Node *np = &n1;
	np->val = n;
	np->left = np->right = 0;
	return np->val;
}
`
	top, _ := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeReferenceResolve())
		DumpReports()
		if len(Reports) != 2 {
			t.Errorf("should have 2 reports")
		}
	}
}

func TestRefs3(t *testing.T) {
	var text = `
struct Grid {
	int expand;
	struct Node {
		int color;
	} *nodes;
};

int foo(int n) {
	struct Grid g;
	return g.nodes[n].color;
}
`
	top, _ := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeReferenceResolve())
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestDetectLoop(t *testing.T) {
	var text = `
struct Tree {
    int payload;
    struct Tree Left, *Right;
} tree;


struct Node2;
struct Node {
	int payload;
	struct Node2 child;
} nd;
struct Node2 {
	struct Node val;
};

struct Level1 {
	struct Level2 {
		struct Level3 {
			struct Level1 parent;
		} t;
	} n;
} i;


typedef struct node my_t;

struct node {
	int val;
	my_t left, right;
};
`
	top, _ := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckLoop())
		if len(Reports) != 5 {
			t.Errorf("some errors are not detected")
		}
		DumpReports()
	}
}

func TestCheckTypes1(t *testing.T) {
	var text = `
int foo() {
	char c = 'b';
    short si = 5;
    long l = 10;
    unsigned short usi = si - c * 2 + l / 2;
	return usi;
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestCheckTypes2(t *testing.T) {
	var text = `
int foo() {
	char *p1, *p2;
	short s = p1 - p2;
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestCheckTypes3(t *testing.T) {
	var text = `
int foo() {
	char *p1, *p2;
	short s = *++p1;
	char c = *(p1 + (p2 - p1));
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestCheckTypes4(t *testing.T) {
	var text = `
struct node {
    long val[4];
    struct data {
        int x, y;
    } data[4];
};

int main()
{
    struct node nd;
	nd.val[1] = nd.data[1].x + nd.data[1].y;
    
    return 0;
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestCheckTypes5(t *testing.T) {
	var text = `
short foo()
{
	char c = 'a';
	return c;
}

int main()
{
    long l = foo();
    return 0;
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestCheckTypes6(t *testing.T) {
	var text = `
short foo(int a, short b, char c[4])
{
	return a + b + c[1];
}

int main()
{
	float a;
	int b;
	char c[4];
    long l = foo(a, b, c);
    return l;
}
`
	top, p := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeCheckTypes())
		p.DumpAst()
		DumpReports()
		if len(Reports) != 0 {
			t.Errorf("should have 0 reports")
		}
	}
}

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
