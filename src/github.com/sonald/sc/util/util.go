// Package util provides utilities
package util

import (
	"log"
	"os"
	"strconv"
	"strings"
)

type Domain int
type Level int

const (
	Scanner Domain = 1 << iota
	Parser
	Sema
	CodeGen
	All = 0xffff
)

var AllowedDomains = int(All)

func (d Domain) String() string {
	switch d {
	case Scanner:
		return "Scanner"

	case Parser:
		return "Parser"
	case Sema:
		return "Sema"
	case CodeGen:
		return "CodeGen"
	}
	return ""
}

const (
	Critical Level = iota
	Warning
	Info
	Debug
	Verbose
)

var AllowedLevel = Warning

type color struct {
	pre  string
	post string
}

var catalogs map[Level]color

// the first two args can be Domain and Level, the default values are
// Domain(all), Level(info)
func Println(v ...interface{}) {
	var dom Domain = All
	var lv Level = Info
	var beg int = 0
	var fmt = ""

	if len(v) > 2 {
		if v, ok := v[0].(Domain); ok {
			dom = v
			beg++
		}

		if v, ok := v[1].(Level); ok {
			lv = v
			beg++
		}
	}

	if (AllowedDomains & int(dom)) == 0 {
		return
	}

	if lv > AllowedLevel {
		return
	}

	fmt = strings.Repeat("%v", len(v)-beg)
	log.Printf(dom.String()+catalogs[lv].pre+fmt+catalogs[lv].post, v[beg:]...)
}

func Printf(v ...interface{}) {
	var dom Domain = All
	var lv Level = Info
	var beg int = 0

	if len(v) >= 3 {
		if v, ok := v[0].(Domain); ok {
			dom = v
			beg++
		}

		if v, ok := v[1].(Level); ok {
			lv = v
			beg++
		}
	}

	if lv > AllowedLevel {
		return
	}

	if (AllowedDomains & int(dom)) == 0 {
		return
	}
	log.Printf(dom.String()+catalogs[lv].pre+v[beg].(string)+catalogs[lv].post, v[beg+1:]...)
}

func init() {
	catalogs = map[Level]color{
		Info:     {"\033[38;5;80m", "\033[00m"},
		Warning:  {"\033[38;5;46m", "\033[00m"},
		Critical: {"\033[38;5;196m", "\033[00m"},
		Debug:    {"\033[38;5;204m", "\033[00m"},
	}

	if dom, ok := os.LookupEnv("SC_DEBUG"); ok {
		if lv, err := strconv.ParseInt(dom, 10, 32); err == nil {
			if lv > int64(Verbose) {
				lv = int64(Verbose)
			}
			AllowedLevel = Level(lv)
			return
		}

		var doms = strings.Split(strings.ToLower(dom), ",")

		AllowedDomains = 0
		for _, d := range doms {
			switch d {
			case "scanner":
				AllowedDomains |= int(Scanner)
			case "parser":
				AllowedDomains |= int(Parser)
			case "sema":
				AllowedDomains |= int(Sema)
			case "codegen":
				AllowedDomains |= int(CodeGen)
			case "all":
				AllowedDomains |= int(All)
			default:
			}
		}

		if AllowedDomains != 0 {
			AllowedLevel = 0xff
		}
	}
}
