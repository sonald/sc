package lexer

import (
	"bytes"
	"fmt"
	"testing"
)

func TestIntConstant(t *testing.T) {
	src := []byte(`
19826;
0x19Abe9;
0XDEADBEEF;
0764523;
023.14;
7428e153;
0xa.1fp20;
13.14E520;

// detect wrong const
28e153.2;
13p12;
13p12e5;
13e14p520;
`)
	s := NewScanner(bytes.NewReader(src))

	fmt.Printf("%d %d\n", KEYWORD, IDENTIFIER)

	expect := map[string]bool{
		"28e153.": true,
		"13p":     true,
		"13e14p":  true,
	}

	for tok := s.Next(); tok.Kind != EOT; tok = s.Next() {
		fmt.Printf("\033[38;5;197mtok: %v\033[00m \n", tok)
		if _, ok := expect[tok.Value.AsString()]; tok.Kind == ERROR && !ok {
			t.Fatalf("invalid token %v", tok)
		}
	}
}

func TestDeclarations(t *testing.T) {
	src := []byte(`
int i = 0xdeedbeef;
const int const *pi = &i;
static const int vi;
int main() {
}`)
	s := NewScanner(bytes.NewReader(src))

	fmt.Printf("%d %d\n", KEYWORD, IDENTIFIER)

	for tok := s.Next(); tok.Kind != EOT; tok = s.Next() {
		fmt.Printf("\033[38;5;199mtok: %v\033[00m \n", tok)
	}
}

func TestWhole(t *testing.T) {
	src := []byte(`
int main() {
	char c = '#';
	char str[] = "hello";

	int ival = 19826;
	/* definition */
	int a, b; // line comment
	return  (a>=b) || ((a<<=1) & b) !=0;
}`)
	s := NewScanner(bytes.NewReader(src))

	fmt.Printf("%d %d\n", KEYWORD, IDENTIFIER)

	for tok := s.Next(); tok.Kind != EOT; tok = s.Next() {
		fmt.Printf("\033[38;5;199mtok: %v\033[00m \n", tok)
	}
}
