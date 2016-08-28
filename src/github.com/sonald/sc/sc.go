package main

import (
	"flag"
	"fmt"
	"github.com/sonald/sc/parser"
	"os"
)

var (
	beVerbose  bool = false
	dumpTokens bool = false
	dumpAst    bool = false
)

func setupFlags() {
	flag.BoolVar(&beVerbose, "verbose", beVerbose, "increase debug output")
	flag.BoolVar(&dumpTokens, "dump-tokens", dumpTokens, "dump tokens scanned")
	flag.BoolVar(&dumpAst, "dump-ast", dumpAst, "dump ast parsed")
}
func main() {
	setupFlags()
	flag.Parse()

	if flag.NArg() == 0 {
		opts := parser.ParseOption{
			Verbose: beVerbose,
		}

		opts.Reader = os.Stdin
		p := parser.NewParser()
		p.Parse(&opts)

		if dumpAst {
			p.DumpAst()
		}
		return
	}

	for _, f := range flag.Args() {
		opts := parser.ParseOption{
			Filename: f,
			Verbose:  beVerbose,
		}

		if r, err := os.Open(f); err == nil {
			opts.Reader = r
			p := parser.NewParser()
			p.Parse(&opts)

			if dumpAst {
				p.DumpAst()
			}
		} else {
			fmt.Errorf("%s", err)
		}
	}

}
