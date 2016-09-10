package main

import (
	"flag"
	"fmt"
	"github.com/sonald/sc/parser"
	"github.com/sonald/sc/sema"
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

func parse(opts *parser.ParseOption) bool {
	p := parser.NewParser()
	var tu = p.Parse(opts)

	sema.RunWalkers(tu)
	sema.DumpReports()
	if dumpAst {
		p.DumpAst()
	}

	if len(sema.Reports) > 0 {
		return false
	}
	return true
}

func main() {
	setupFlags()
	flag.Parse()

	if flag.NArg() == 0 {
		opts := parser.ParseOption{
			Verbose: beVerbose,
		}

		opts.Reader = os.Stdin
		if !parse(&opts) {
			return
		}
	}

	for _, f := range flag.Args() {
		opts := parser.ParseOption{
			Filename: f,
			Verbose:  beVerbose,
		}

		if r, err := os.Open(f); err == nil {
			opts.Reader = r
			if !parse(&opts) {
				return
			}

		} else {
			fmt.Errorf("%s", err)
		}
	}

}
