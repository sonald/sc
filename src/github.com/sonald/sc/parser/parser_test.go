package parser

import (
	"github.com/sonald/sc/lexer"
	"os"
	"testing"
)

func TestSymbols(t *testing.T) {
	sym := Symbol{lexer.Token{Kind: lexer.IDENTIFIER}, &IntegerType{}, Auto}
	t.Logf("sym: %s\n", sym)

	sym = Symbol{
		lexer.Token{Kind: lexer.IDENTIFIER},
		&Function{
			&Pointer{&IntegerType{}},
			[]SymbolType{&IntegerType{}, &Pointer{&FloatType{}}}},
		Auto,
	}
	t.Logf("sym: %s\n", sym)
	/*
		int* grid[10][8];
	*/

	var aty = &Array{
		&Pointer{&IntegerType{}},
		2,
		[]int{10, 8},
	}
	t.Logf("array -> %v\n", aty)

	/*
		strcut Node {
			int val;
			struct Node *left, *right;
		}

		struct Node *root;
	*/

	var ty *Struct = &Struct{}
	ty.Name = "Node"
	ty.Fields = []SymbolType{
		&IntegerType{},
		&Pointer{ty},
		&Pointer{ty},
	}

	var s = Symbol{
		lexer.Token{Kind: lexer.IDENTIFIER},
		ty,
		Auto,
	}

	t.Logf("sym %v\n", s)
}

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

func TestParseFile(t *testing.T) {
	opts := ParseOption{
		filename: "./test.txt",
	}

	p := NewParser()
	p.Parse(&opts)

	p.DumpSymbols()
}
