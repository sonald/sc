package sema

import (
	"flag"
	"github.com/sonald/sc/ast"
	"github.com/sonald/sc/parser"
	"os"
	"strings"
	"testing"
)

func testTemplate(t *testing.T, text string) ast.Ast {
	opts := parser.ParseOption{
		Filename: "./test.txt",
		Verbose:  true,
	}

	opts.Reader = strings.NewReader(text)
	p := parser.NewParser()
	top := p.Parse(&opts)

	p.DumpAst()
	return top
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
	top := testTemplate(t, text)
	if top == nil {
		t.Errorf("parse failed")
	} else {
		ast.WalkAst(top, MakeReferenceResolve())
	}
}

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(m.Run())
}
