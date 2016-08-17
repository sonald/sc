package parser

import (
	"fmt"
	"github.com/sonald/sc/lexer"
	"strings"
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

func isSimpleType(s SymbolType) bool {
	switch s.(type) {
	case *VoidType, *IntegerType, *FloatType, *DoubleType:
		return true
	default:
		return false
	}
}

// decorate base type with qualifier
type QualifiedType struct {
	Base SymbolType
	Qualifier
}

func (q *QualifiedType) String() string {
	return fmt.Sprintf("(%s %s)", q.Qualifier, q.Base)
}

type VoidType struct {
}

func (v *VoidType) String() string {
	return "void"
}

//char, short, int, long, long long
type IntegerType struct {
	Unsigned bool
	Kind     string
}

func (i *IntegerType) String() string {
	var s = ""
	if i.Unsigned {
		s += "unsigned "
	}

	return s + i.Kind
}

type FloatType struct {
}

func (i *FloatType) String() string {
	return "float"
}

type DoubleType struct {
}

func (i *DoubleType) String() string {
	return "double"
}

type Pointer struct {
	Source SymbolType
}

func (p *Pointer) String() string {
	switch p.Source.(type) {
	case *RecordType:
		var s = p.Source.(*RecordType)
		if s.Union {
			return fmt.Sprintf("union %s*", s.Name)
		}
		return fmt.Sprintf("struct %s*", s.Name)
	default:
		if isSimpleType(p.Source) {
			return fmt.Sprintf("%v*", p.Source)
		} else {
			return fmt.Sprintf("(%v*)", p.Source)
		}
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
	var s string
	if isSimpleType(a.ElemType) {
		s = fmt.Sprintf("%v", a.ElemType)
	} else {
		s = fmt.Sprintf("(%v)", a.ElemType)
	}

	for i := 0; i < a.Level; i++ {
		if a.Lens[i] == -1 {
			s += fmt.Sprintf("[]")
		} else {
			s += fmt.Sprintf("[%d]", a.Lens[i])
		}
	}
	return s
}

var anonymousRecordSeq int = 0
var anonymousFieldSeq int = 0

type FieldType struct {
	Base SymbolType
	Name string
	Tag  *int // a pointer since Tag is optional
}

func (ft *FieldType) String() string {
	nm := ft.Name
	if strings.HasPrefix(ft.Name, "!") {
		nm = ""
	}
	if ft.Tag != nil {
		return fmt.Sprintf("%v %s:%d", ft.Base, nm, *ft.Tag)
	}
	return fmt.Sprintf("%v %s", ft.Base, nm)
}

func NextAnonyFieldName(recName string) string {
	anonymousFieldSeq++
	return fmt.Sprintf("!%s!field%d", recName, anonymousFieldSeq)
}

// check loop def in semantic module
// anonymous record got a name leading with ! (because this is a letter that won't
// be accepted by compiler
type RecordType struct {
	Name   string // record name or compiler assigned internal name for anonymous record
	Union  bool
	Fields []*FieldType
}

func (s *RecordType) String() string {
	kd := "struct"
	if s.Union {
		kd = "union"
	}

	nm := s.Name
	if strings.HasPrefix(nm, "!recordty") {
		nm = ""
	}
	var str = fmt.Sprintf("%s %s{", kd, s.Name)
	for i, f := range s.Fields {
		str += fmt.Sprintf("%s", f)
		if i < len(s.Fields)-1 {
			str += "; "
		}
	}
	str += "}"
	return str
}

func NextAnonyRecordName() string {
	anonymousRecordSeq++
	return fmt.Sprintf("!recordty%d", anonymousRecordSeq)
}

var anonymousEnumSeq = 0

type EnumType struct {
	Loc lexer.Location
}

func (e *EnumType) String() string {
	return ""
}

func NextAnonyEnumName() string {
	anonymousEnumSeq++
	return fmt.Sprintf("!enumty%d", anonymousEnumSeq)
}

// typedef
type UserType struct {
	Ref SymbolType
	Loc lexer.Location
}

func (s *UserType) String() string {
	panic("not implemented")
	return "UserType"
}

// we actually have two kinds of symbols
// first is normal simple reside in VariableDecl or FunctionDecl
// the second is type symbol for user defined type names
type Symbol struct {
	Name   lexer.Token
	Type   SymbolType
	Custom bool // true if a type symbol
	Storage
}

func (sym *Symbol) String() string {
	var s = ""
	if sym.Storage != NilStorage {
		s += fmt.Sprint(sym.Storage) + " "
	}

	return fmt.Sprintf("%v%v %v", s, sym.Type, sym.Name.AsString())
}

var dummyVariableCounter = 0

func NextDummyVariableName() string {
	dummyVariableCounter++
	return fmt.Sprintf("!dummyVar%d", dummyVariableCounter)
}

type SymbolScope struct {
	Symbols  []*Symbol
	types    []SymbolType
	Parent   *SymbolScope
	Children []*SymbolScope
	//FIXME: this will introduce a ref-loop, not gc friendly
	Owner Ast
}

func (scope *SymbolScope) AddSymbol(sym *Symbol) {
	scope.Symbols = append(scope.Symbols, sym)
}

func (scope *SymbolScope) LookupSymbol(name string, customed bool) *Symbol {
	var current = scope

	for ; current != nil; current = current.Parent {
		for _, sym := range current.Symbols {
			if sym.Custom == customed && sym.Name.AsString() == name {
				return sym
			}
		}
	}

	return nil
}

func (scope *SymbolScope) RegisterUserType(st SymbolType) {
	switch st.(type) {
	case *RecordType:
		rt := st.(*RecordType)
		if prev := scope.LookupUserType(rt.Name); prev != nil {
			var s = [2]string{"type redeclartion, previous is at ", ""}

			switch prev.(type) {
			case *RecordType:
				//FIXME: need to find corresponding RecordDecl
				// s[1] = fmt.Sprint(prev.(*RecordType).Loc)
			case *EnumType:
			case *UserType:
				break
			}
			panic(fmt.Sprintf(s[0], s[1]))

		}

		scope.types = append(scope.types, st)
	case *EnumType:
	case *UserType:
		break
	}
}

func (scope *SymbolScope) LookupUserType(name string) SymbolType {
	for _, st := range scope.types {
		switch st.(type) {
		case *RecordType:
			rt := st.(*RecordType)
			if rt.Name == name {
				return rt
			}
		case *EnumType:
		case *UserType:
			break

		default:
			panic("invalid type for usertype")
		}
	}

	return nil
}
