package parser

import (
	"flag"
	"github.com/sonald/sc/lexer"
	"os"
	"strings"
	"testing"
)

func TestParseTokens(t *testing.T) {
	opts := ParseOption{
		filename: "./test.txt",
	}

	p := NewParser()
	if f, err := os.Open(opts.filename); err == nil {
		p.lex = lexer.NewScanner(f)
	}

	for i := 0; i < NR_LA; i++ {
		p.tokens[i] = p.getNextToken()
	}

	for {
		if p.next().Kind == lexer.EOT {
			break
		}
	}

}

func TestParseScope(t *testing.T) {
}

func TestParseDecls(t *testing.T) {
	var text = `
static const int *id, *id2;
register int global_var;

static const int add(const int *a, const int b);

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

`
	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	if opts.dumpSymbols {
		p.DumpSymbols()
	}
	p.DumpAst()

}

func TestDetectLoop(t *testing.T) {
	var text = `
struct Tree {
    int payload;
    struct Tree Left, *Right;
} tree;

`
	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	if opts.dumpSymbols {
		p.DumpSymbols()
	}
	p.DumpAst()

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

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()
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

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()
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

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()
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
}
`

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()
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

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()

	if opts.dumpSymbols {
		p.DumpSymbols()
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

	opts := ParseOption{
		filename: "./test.txt",
		verbose:  true,
	}

	opts.reader = strings.NewReader(text)
	p := NewParser()
	p.Parse(&opts)

	p.DumpAst()

	if opts.dumpSymbols {
		p.DumpSymbols()
	}
}

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
