package lexer

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
)

type Kind int

const (
	IDENTIFIER Kind = 129 + iota
	KEYWORD
	STR_LITERAL
	INT_LITERAL
	FLOAT_LITERAL
	CHAR_LITERAL
	LPAREN // 135
	RPAREN
	OPEN_BRACKET
	CLOSE_BRACKET
	LBRACE
	RBRACE    // 140
	COLON     // :
	COMMA     // ,
	SEMICOLON // ;
	DOT       // .
	REFERENCE // ->
	INC       // --
	DEC       // ++
	AND       // &
	OR        // |
	XOR       // ^
	LOG_AND   // &&
	LOG_OR    // ||
	QUEST     // ?
	ELLIPSIS  // ...
	PLUS
	MINUS
	MUL
	DIV
	MOD    // %
	NOT    // !
	TILDE  // ~
	RSHIFT // >>
	LSHIFT // <<
	LESS
	GREAT
	EQUAL // ==
	LE    // <=
	GE    // >=
	NE    // !=
	ASSIGN
	MUL_ASSIGN
	DIV_ASSIGN
	MOD_ASSIGN
	PLUS_ASSIGN
	MINUS_ASSIGN
	LSHIFT_ASSIGN
	RSHIFT_ASSIGN
	AND_ASSIGN
	XOR_ASSIGN
	OR_ASSIGN

	LINE_COMMENT
	BLOCK_COMMENT

	ERROR
	//for preprocessor
	//#
	//##
	EOT
)

const eof = 255

var keywords map[string]bool
var TokKinds map[Kind]string

type Location struct {
	Offset int64
	Line   int
	Column int
}

type Value struct {
	content string
}

func (self Value) AsString() string {
	return self.content
}

func (self Value) AsInt() int {
	if i, err := strconv.ParseInt(self.content, 0, 32); err != nil {
		panic("Can not convert to int")
	} else {
		return int(i)
	}

}

func (self Value) AsChar() byte {
	if len(self.content) != 1 {
		panic("Can not convert to char")
	}
	return self.content[0]
}

type Token struct {
	Kind
	Location
	Value
}

func MakeToken(kd Kind, val string) Token {
	tok := Token{Kind: kd}
	tok.Value.content = val
	return tok
}

func (self Token) String() string {
	return fmt.Sprintf("{%s, Loc: %v, V: [%s]}",
		TokKinds[self.Kind], self.Location, self.Value.AsString())
}

type Scanner struct {
	start       int64  // start of next token
	offset      int64  // total offset in source file
	lines       int    // current line
	cols        int    // col in a line
	precols     int    // previous col across a line
	val         []byte // lexical value
	tokens      chan Token
	reader      *bufio.Reader
	EmitComment bool
}

type StateFn func(*Scanner) StateFn

func NewScanner(r io.Reader) *Scanner {
	s := &Scanner{
		reader: bufio.NewReader(r),
		val:    make([]byte, 0, 32),
		tokens: make(chan Token),
		lines:  1,
		cols:   0,
	}
	go s.run()

	return s
}

func inside(group []byte, c byte) bool {
	return bytes.IndexByte(group, c) >= 0
}

func (self *Scanner) acceptOne(group []byte) bool {
	if bytes.IndexByte(group, self.peek()) >= 0 {
		self.next()
		return true
	}

	return false
}

func (self *Scanner) accept(group []byte) {
	for bytes.IndexByte(group, self.next()) >= 0 {
	}
	self.backup() // retreat unmatched byte
}

// skip until a seq point found
func (self *Scanner) recover() {
	for self.peek() != eof && bytes.IndexByte([]byte("}]);"), self.next()) < 0 {
	}
}

func (self *Scanner) skipSpaces() {
}

func (self *Scanner) next() byte {
	c, err := self.reader.ReadByte()
	if err != nil {
		if err == io.EOF {
			return eof
		} else {
			panic(err.Error())
		}
	}

	self.offset++
	self.cols++
	if c == '\n' {
		self.lines++
		self.precols = self.cols
		self.cols = 0
	}
	self.val = append(self.val, c)
	return c
}

func (self *Scanner) peek() byte {
	if b, err := self.reader.Peek(1); err != nil {
		if err == io.EOF {
			return eof
		} else {
			panic(err.Error())
		}
	} else {
		return b[0]
	}
}

//unget current byte into stream
//NOTE: backup can only do once, since UnreadByte can not be called
//more than once consecutively at a time
func (self *Scanner) backup() {
	self.offset--
	if len(self.val) > 0 && self.val[len(self.val)-1] == '\n' {
		self.lines--
		self.cols = self.precols
	} else {
		self.cols--
	}
	self.val = self.val[:len(self.val)-1]
	self.reader.UnreadByte()
}

func (self *Scanner) emit(kd Kind) {
	tok := Token{
		Kind:     kd,
		Location: Location{Offset: self.start, Line: self.lines, Column: self.cols - len(self.val)},
		Value:    Value{string(self.val)},
	}
	self.start = 0
	self.val = self.val[:0]

	//log.Printf("emit %v\n", tok)
	self.tokens <- tok
}

func isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

func isBlank(c byte) bool {
	return bytes.IndexByte([]byte(" \t\n\v\r"), c) != -1
}

func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func isAlphaNum(c byte) bool {
	return isAlpha(c) || isDigit(c)
}

func isNumPrefix(c byte) bool {
	return true
}

func start(self *Scanner) StateFn {
	//log.Println("start")

	c := self.next()

	for ; isBlank(c); c = self.next() {
	}

	if isDigit(c) {
		self.backup()
		return stateIntConstant
	}

	if isAlpha(c) || c == '_' {
		self.backup()
		return stateIdentifier
	}

	switch c {
	case '\'':
		return stateCharConstant
	case '"':
		return stateStrConstant

	case '/':
		switch self.peek() {
		case '/':
			self.next()
			return stateLineComment
		case '*':
			self.next()
			return stateBlockComment
		default:
			self.backup()
			return statePunctuator
		}

	case eof:
		self.emit(EOT)

	default:
		for ; isBlank(c); c = self.next() {
		}
		self.backup()
		return statePunctuator
	}
	return nil
}

//FIXME: how to recover ?
func stateError(self *Scanner) StateFn {
	self.emit(ERROR)
	self.recover()
	return start
}

//FIXME: detect more wrong constants
//FIXME: support integer-suffix (see N1256)
func stateIntConstant(self *Scanner) StateFn {
	self.start = self.offset
	self.val = self.val[:0]

	group := []byte("0123456789")

	c := self.next()
	if c == '0' {
		switch self.peek() {
		case 'x', 'X': // hexadecimal
			group = []byte("0123456789abcdefABCDEF")
			self.next()
		default:
			group = []byte("01234567")

		}
	}

	//log.Printf("stateIntConstant group %s", group)
	self.accept(group)

	if self.acceptOne([]byte(".")) {
		self.accept(group)
	}

	has_exp := false
	exp := "eE"
	if inside(group, 'A') {
		exp = "pP"
	}
	if self.acceptOne([]byte(exp)) {
		has_exp = true
		self.accept(group)
	}

	if self.peek() == '.' ||
		(has_exp && inside([]byte("eEpP"), self.peek())) ||
		(exp[0] == 'p' && inside([]byte("eE"), self.peek())) ||
		(exp[0] == 'e' && inside([]byte("pP"), self.peek())) {
		self.next()
		return stateError
	}

	self.emit(INT_LITERAL)
	return start
}

func stateCharConstant(self *Scanner) StateFn {
	self.start = self.offset - 1
	self.val = self.val[:0]

	//FIXME: handle escape \'
	self.next() // eat char

	if self.next() != '\'' {
		self.emit(ERROR)
		return nil
	}

	self.val = self.val[:len(self.val)-1]
	self.emit(CHAR_LITERAL)
	return start
}

func stateStrConstant(self *Scanner) StateFn {
	self.start = self.offset - 1
	self.val = self.val[:0]

	//FIXME: handle escape seq \"
	for {
		if c := self.next(); c == '"' {
			break
		}
	}

	self.val = self.val[:len(self.val)-1]
	self.emit(STR_LITERAL)
	return start
}

//FIXME: handle line escape  with '\' at the end of line
func stateLineComment(self *Scanner) StateFn {
	self.start = self.offset - 2
	self.val = self.val[:0]
	for {
		c := self.next()
		if c == '\n' || c == eof {
			self.backup()
			break
		}
	}

	if self.EmitComment {
		self.emit(LINE_COMMENT)
	}
	return start
}

func stateBlockComment(self *Scanner) StateFn {
	//log.Println("stateBlockComment")
	self.start = self.offset - 2
	self.val = self.val[:0]
	for {
		c := self.next()
		if c == eof {
			self.backup()
			break
		}

		if c == '*' && self.next() == '/' {
			break
		}
	}
	self.val = self.val[:len(self.val)-2]
	if self.EmitComment {
		self.emit(BLOCK_COMMENT)
	}
	return start
}

func isPunctuatorPrefix(c byte) bool {
	opts := []byte("[](){}.->+&|*~!/%<>=^?:;#")

	return bytes.IndexByte(opts, c) != -1
}

func statePunctuator(self *Scanner) StateFn {
	//log.Println("statePunctuator")

	self.start = self.offset
	self.val = self.val[:0]

	c := self.next()

	switch c {
	//case '#':
	case '>':
		if self.peek() == '>' {
			self.next()
			if self.peek() == '=' {
				self.next()
				self.emit(RSHIFT_ASSIGN)
			} else {
				self.emit(RSHIFT)
			}
		} else if self.peek() == '=' {
			self.next()
			self.emit(GE)
		} else {
			self.emit(GREAT)
		}
	case '<':
		if self.peek() == '<' {
			self.next()
			if self.peek() == '=' {
				self.next()
				self.emit(LSHIFT_ASSIGN)
			} else {
				self.emit(LSHIFT)
			}
		} else if self.peek() == '=' {
			self.next()
			self.emit(LE)
		} else {
			self.emit(LESS)
		}

	case '=':
		if self.peek() == '=' {
			self.next()
			self.emit(EQUAL)
		} else {
			self.emit(ASSIGN)
		}

	case '^':
		if self.peek() == '=' {
			self.next()
			self.emit(XOR_ASSIGN)
		} else {
			self.emit(XOR)
		}
	case '%':
		if self.peek() == '=' {
			self.next()
			self.emit(MOD_ASSIGN)
		} else {
			self.emit(MOD)
		}
	case '*':
		if self.peek() == '=' {
			self.next()
			self.emit(MUL_ASSIGN)
		} else {
			self.emit(MUL)
		}
	case '/':
		if self.peek() == '=' {
			self.next()
			self.emit(DIV_ASSIGN)
		} else {
			self.emit(DIV)
		}
	case '!':
		if self.peek() == '=' {
			self.next()
			self.emit(NE)
		} else {
			self.emit(NOT)
		}

	case '|':
		switch self.next() {
		case '|':
			self.emit(LOG_OR)
		case '=':
			self.emit(OR_ASSIGN)
		default:
			self.backup()
			self.emit(OR)
		}
	case '&':
		switch self.next() {
		case '&':
			self.emit(LOG_AND)
		case '=':
			self.emit(AND_ASSIGN)
		default:
			self.backup()
			self.emit(AND)
		}
	case '-':
		switch self.next() {
		case '>':
			self.emit(REFERENCE)
		case '-':
			self.emit(DEC)
		case '=':
			self.emit(MINUS_ASSIGN)
		default:
			self.backup()
			self.emit(MINUS)
		}

	case '+':
		switch self.next() {
		case '+':
			self.emit(INC)
		case '=':
			self.emit(PLUS_ASSIGN)
		default:
			self.backup()
			self.emit(PLUS)
		}

	case '{':
		self.emit(LBRACE)
	case '}':
		self.emit(RBRACE)
	case '[':
		self.emit(OPEN_BRACKET)
	case ']':
		self.emit(CLOSE_BRACKET)
	case '(':
		self.emit(LPAREN)
	case ')':
		self.emit(RPAREN)

	case '.':
		if self.next() == '.' {
			if self.next() == '.' {
				self.emit(ELLIPSIS)
			} else {
				self.emit(ERROR)
			}
		} else {
			self.backup()
			self.emit(DOT)
		}

	case '?':
		self.emit(QUEST)
	case ',':
		self.emit(COMMA)
	case ':':
		self.emit(COLON)
	case ';':
		self.emit(SEMICOLON)
	case '~':
		self.emit(TILDE)
	}

	return start
}

// id or keywords
func stateIdentifier(self *Scanner) StateFn {
	//log.Println("stateIdentifier")

	self.start = self.offset
	self.val = self.val[:0]
	for {
		if c := self.next(); !(isAlphaNum(c) || c == '_') {
			self.backup()

			if _, ok := keywords[string(self.val)]; ok {
				self.emit(KEYWORD)
			} else {
				self.emit(IDENTIFIER)
			}

			break
		}
	}

	return start
}

func (self *Scanner) run() {
	for fn := start; fn != nil; {
		fn = fn(self)
	}

	close(self.tokens)
}

// read current token
func (self *Scanner) Peek() Token {
	return Token{}
}

// return current token and advance and read in next token
func (self *Scanner) Next() Token {
	for {
		select {
		case tok := <-self.tokens:
			return tok
		default:
			// nothing, just polling
		}
	}
}

func init() {
	kws := []string{
		"auto", "break", "case", "char", "const", "continue", "default", "do", "double",
		"else", "enum", "extern", "float", "for", "goto", "if", "inline", "int", "long",
		"register", "restrict", "return", "short", "signed", "sizeof", "static", "struct",
		"switch", "typedef", "union", "unsigned", "void", "volatile", "while",
	}

	keywords = make(map[string]bool)
	for _, kw := range kws {
		keywords[kw] = true
	}

	TokKinds = map[Kind]string{
		IDENTIFIER:    "IDENTIFIER",
		KEYWORD:       "KEYWORD",
		STR_LITERAL:   "STR_LITERAL",
		INT_LITERAL:   "INT_LITERAL",
		FLOAT_LITERAL: "FLOAT_LITERAL",
		CHAR_LITERAL:  "CHAR_LITERAL",
		LPAREN:        "(",
		RPAREN:        ")",
		OPEN_BRACKET:  "[",
		CLOSE_BRACKET: "]",
		LBRACE:        "{",
		RBRACE:        "}",
		COLON:         ":",
		COMMA:         ",",
		SEMICOLON:     ";",
		DOT:           ".",
		REFERENCE:     "->",
		INC:           "++",
		DEC:           "--",
		AND:           "&",
		OR:            "|",
		XOR:           "^",
		LOG_AND:       "&&",
		LOG_OR:        "||",
		QUEST:         "?",
		ELLIPSIS:      "...",
		PLUS:          "+",
		MINUS:         "-",
		MUL:           "*",
		DIV:           "/",
		MOD:           "%",
		NOT:           "!",
		TILDE:         "~",
		RSHIFT:        ">>",
		LSHIFT:        "<<",
		LESS:          "<",
		GREAT:         ">",
		EQUAL:         "==",
		LE:            "<=",
		GE:            ">=",
		NE:            "!=",
		ASSIGN:        "=",
		MUL_ASSIGN:    "*=",
		DIV_ASSIGN:    "/=",
		MOD_ASSIGN:    "%=",
		PLUS_ASSIGN:   "+=",
		MINUS_ASSIGN:  "-=",
		LSHIFT_ASSIGN: "<<=",
		RSHIFT_ASSIGN: ">>=",
		AND_ASSIGN:    "&=",
		XOR_ASSIGN:    "^=",
		OR_ASSIGN:     "|=",
		LINE_COMMENT:  "//",
		BLOCK_COMMENT: "BLOCK_COMMENT",
		ERROR:         "ERROR",
		EOT:           "EOT",
	}
}
