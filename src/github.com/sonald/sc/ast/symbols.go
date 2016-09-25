package ast

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
	if _, yes := q.Base.(*Pointer); yes {
		return fmt.Sprintf("%s %s", q.Base, q.Qualifier)
	} else {
		return fmt.Sprintf("%s %s", q.Qualifier, q.Base)
	}
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
	Return     SymbolType
	Args       []SymbolType
	IsVariadic bool
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
	if f.IsVariadic {
		s += ",..."
	}
	s += ")"

	return s
}

type Array struct {
	ElemType SymbolType
	Level    int
	//NOTE: this is a little odd and inpure..., can I come up with
	// another elegant solution?
	LenExprs []Expression
}

func (a *Array) String() string {
	var s string
	if isSimpleType(a.ElemType) {
		s = fmt.Sprintf("%v", a.ElemType)
	} else {
		s = fmt.Sprintf("(%v)", a.ElemType)
	}

	for i := 0; i < a.Level; i++ {
		s += fmt.Sprintf("[]")
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
	// there could be a loop, so do not print field types, only names
	for i, f := range s.Fields {
		str += fmt.Sprintf("%s", f.Name)
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

type EnumeratorType struct {
	Name string
}

func (e *EnumeratorType) String() string {
	return fmt.Sprintf("%v", e.Name)
}

type EnumType struct {
	Name string
	List []*EnumeratorType
}

func (e *EnumType) String() string {
	return fmt.Sprintf("enum %s", e.Name)
}

func NextAnonyEnumName() string {
	anonymousEnumSeq++
	return fmt.Sprintf("!enumty%d", anonymousEnumSeq)
}

// typedef
type UserType struct {
	Name string
	Ref  SymbolType
}

func (s *UserType) String() string {
	return fmt.Sprintf("%s := %v", s.Name, s.Ref)
}

type LabelType struct {
	Name string
}

func (s *LabelType) String() string {
	return fmt.Sprintf("Label(%s)", s.Name)
}

type SymbolNamespace int

const (
	OrdinaryNS SymbolNamespace = iota
	LabelNS
	TagNS    // for tag of records & enums
	MemberNS // for members of records & enums, and these symbols are stored in SymbolScope related to owner
	AnyNS    // used for lookup
)

type Symbol struct {
	Name lexer.Token
	Type SymbolType
	Storage
	NS SymbolNamespace
}

func (sym *Symbol) String() string {
	var s = fmt.Sprintf("'%v' %v", sym.Type, sym.Name.AsString())
	if sym.Storage != NilStorage {
		s += " " + fmt.Sprint(sym.Storage)
	}
	return s
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
	for _, sym2 := range scope.Symbols {
		if sym2.NS == sym.NS && sym2.Name.AsString() == sym.Name.AsString() {
			panic(fmt.Sprintf("redeclaration of %s, previous is at %d:%d", sym.Name.AsString(),
				sym2.Name.Line, sym2.Name.Column))
		}
	}
	scope.Symbols = append(scope.Symbols, sym)
}

func (scope *SymbolScope) LookupSymbol(name string, ns SymbolNamespace) *Symbol {
	var current = scope

	for ; current != nil; current = current.Parent {
		for _, sym := range current.Symbols {
			if (sym.NS == ns || ns == AnyNS) && sym.Name.AsString() == name {
				return sym
			}
		}
	}

	return nil
}

type SymbolPredicate func(*Symbol) bool

func (scope *SymbolScope) LookupSymbolsBy(predicate SymbolPredicate) (ret []*Symbol) {
	var current = scope

	for ; current != nil; current = current.Parent {
		for _, sym := range current.Symbols {
			if predicate(sym) {
				ret = append(ret, sym)
			}
		}
	}

	return
}

//FIXME: record and typedef are two distinct name spaces
func (scope *SymbolScope) RegisterNamedType(st SymbolType) {
	var (
		name string
		ns   SymbolNamespace
	)

	switch st.(type) {
	case *RecordType:
		rt := st.(*RecordType)
		name = rt.Name
		ns = TagNS

	case *EnumType:
		et := st.(*EnumType)
		name = et.Name
		ns = TagNS

	case *EnumeratorType:
		et := st.(*EnumeratorType)
		name = et.Name
		ns = OrdinaryNS

	case *UserType:
		ut := st.(*UserType)
		name = ut.Name
		ns = OrdinaryNS

	case *LabelType:
		name = st.(*LabelType).Name
		ns = LabelNS
	}

	if prev := scope.LookupNamedType(name, ns); prev != nil {
		var s = [2]string{"type redeclartion, previous is at ", ""}
		panic(fmt.Sprintf(s[0], s[1]))

	} else {
		scope.types = append(scope.types, st)
	}
}

func (scope *SymbolScope) LookupNamedTypeRecursive(name string, ns SymbolNamespace) SymbolType {
	var current = scope

	for ; current != nil; current = current.Parent {
		if ty := current.LookupNamedType(name, ns); ty != nil {
			return ty
		}
	}
	return nil
}

func (scope *SymbolScope) LookupNamedType(name string, ns SymbolNamespace) SymbolType {
	var match = func(desired SymbolNamespace) bool {
		return ns == AnyNS || ns == desired
	}

	for _, st := range scope.types {
		switch st.(type) {
		case *RecordType:
			rt := st.(*RecordType)
			if match(TagNS) && rt.Name == name {
				return rt
			}
		case *EnumeratorType:
			et := st.(*EnumeratorType)
			if match(OrdinaryNS) && et.Name == name {
				return et
			}
		case *EnumType:
			et := st.(*EnumType)
			if match(TagNS) && et.Name == name {
				return et
			}
		case *UserType:
			ut := st.(*UserType)
			if match(OrdinaryNS) && ut.Name == name {
				return ut
			}

		case *LabelType:
			lt := st.(*LabelType)
			if match(LabelNS) && lt.Name == name {
				return lt
			}

		default:
			panic("invalid type for namedtype")
		}
	}

	return nil
}

var Storages map[string]Storage

var TypeSpecifier map[string]bool

var TypeQualifier map[string]Qualifier

func IsStorageClass(tok lexer.Token) bool {
	_, ok := Storages[tok.AsString()]
	return ok
}

func IsTypeSpecifier(tok lexer.Token) bool {
	_, ok := TypeSpecifier[tok.AsString()]
	return ok
}

func IsTypeQualifier(tok lexer.Token) bool {
	_, ok := TypeQualifier[tok.AsString()]
	return ok
}

func init() {
	Storages = make(map[string]Storage)
	Storages["auto"] = Auto
	Storages["static"] = Static
	Storages["extern"] = External
	Storages["register"] = Register
	Storages["typedef"] = Typedef

	TypeSpecifier = make(map[string]bool)
	var ts = [...]string{"void", "char", "short", "int", "long", "float",
		"double", "signed", "unsigned", "struct", "union", "enum"}
	for _, v := range ts {
		TypeSpecifier[v] = true
	}

	TypeQualifier = make(map[string]Qualifier)
	TypeQualifier["const"] = Const
	TypeQualifier["restrict"] = Restrict
	TypeQualifier["volatile"] = Volatile
}
