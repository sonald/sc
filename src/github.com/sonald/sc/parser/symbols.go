package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
)

type Storage int

const (
	NilStorage Storage = 0
	Auto       Storage = 1 << iota
	Static
	External
	Register
	Typedef
)

type Qualifier int

const (
	NilQualifier Qualifier = 0
	Const        Qualifier = 1 << iota
	Volatile
	Restrict
)

func (s Storage) String() string {
	switch s {
	case Auto:
		return "auto"
	case Static:
		return "static"
	case External:
		return "external"
	case Register:
		return "register"
	case Typedef:
		return "typedef"
	}
	return ""
}

func (q Qualifier) String() string {
	switch q {
	case Const:
		return "const"
	case Volatile:
		return "volatile"
	case Restrict:
		return "restrict"
	}
	return ""
}

type SymbolType interface {
	String() string
}

type QualifiedType struct {
	Base SymbolType
	Qualifier
}

func (q *QualifiedType) String() string {
	return fmt.Sprintf("%s %s", q.Qualifier, q.Base)
}

type IntegerType struct {
}

func (i *IntegerType) String() string {
	return "int"
}

type FloatType struct {
}

func (i *FloatType) String() string {
	return "float"
}

type Pointer struct {
	Source SymbolType
}

func (p *Pointer) String() string {
	switch p.Source.(type) {
	case *Struct:
		var s = p.Source.(*Struct)
		return fmt.Sprintf("struct %s*", s.Name)
	default:
		return fmt.Sprintf("%v*", p.Source)
	}
}

type Function struct {
	Return SymbolType
	Args   []SymbolType
}

func (f *Function) String() string {
	s := fmt.Sprintf("%v ", f.Return)
	s += "("
	for i, arg := range f.Args {
		s += fmt.Sprint(arg)
		if i < len(f.Args)-1 {
			s += ", "
		}
	}
	s += ")"

	return s
}

type Array struct {
	ElemType SymbolType
	Level    int
	Lens     []int // length of each level
}

func (a *Array) String() string {
	var s = fmt.Sprintf("%v ", a.ElemType)
	for i := 0; i < a.Level; i++ {
		s += fmt.Sprintf("[%d]", a.Lens[i])
	}
	return s
}

// check loop def in semantic module
type Struct struct {
	Name   string
	Fields []SymbolType
}

func (s *Struct) String() string {
	var str = fmt.Sprintf("struct %s{", s.Name)
	for i, f := range s.Fields {
		str += fmt.Sprintf("%s", f)
		if i < len(s.Fields)-1 {
			str += ", "
		}
	}
	str += "}"
	return str
}

// typedef
type UserType struct {
	Ref SymbolType
}

func (s *UserType) String() string {
	panic("not implemented")
	return "UserType"
}

type Symbol struct {
	Name lexer.Token
	Type SymbolType
	Storage
}

func (sym *Symbol) String() string {
	var s = ""
	if sym.Storage != NilStorage {
		s += fmt.Sprint(sym.Storage) + " "
	}

	return fmt.Sprintf("%v%v %v", s, sym.Type, sym.Name.AsString())
}

type SymbolScope struct {
	Symbols  []*Symbol
	Parent   *SymbolScope
	Children []*SymbolScope
}

func InitTopScope() *SymbolScope {
	return nil
}

func (s *SymbolScope) AddSymbol(name string) *Symbol {
	return nil
}

func (s *SymbolScope) LookupSymbol(name string) *Symbol {
	return nil
}
