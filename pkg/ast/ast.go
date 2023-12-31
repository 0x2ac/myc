package ast

import (
	"fmt"
	"strings"

	"github.com/kartiknair/myc/pkg/token"
)

type Type interface {
	isType()
	Equals(Type) bool
	String() string
	IsCopyable() bool
	Methods() *map[string]FunctionType
}

type PrimitiveKind int

const (
	I8 PrimitiveKind = iota
	I16
	I32
	I64

	U8
	U16
	U32
	U64

	F32
	F64

	Str
	Bool
)

var PrimitiveKindStrings = [...]string{
	I8:  "i8",
	I16: "i16",
	I32: "i32",
	I64: "i64",

	U8:  "u8",
	U16: "u16",
	U32: "u32",
	U64: "u64",

	F32: "f32",
	F64: "f64",

	Str:  "str",
	Bool: "bool",
}

func IsPrimitive(name string) bool {
	for _, kindName := range PrimitiveKindStrings {
		if name == kindName {
			return true
		}
	}

	return false
}

// Panics if provided string is not a valid primitive kind
func PrimitiveKindFromString(name string) PrimitiveKind {
	for kind, kindName := range PrimitiveKindStrings {
		if name == kindName {
			return PrimitiveKind(kind)
		}
	}

	panic("Invalid primitive name passed to `PrimitiveKindFromString`")
}

func (p PrimitiveKind) String() string {
	return PrimitiveKindStrings[p]
}

func (p PrimitiveKind) IsSignedInteger() bool {
	return p >= I8 && p <= I64
}

func (p PrimitiveKind) IsUnsignedInteger() bool {
	return p >= U8 && p <= U64
}

func (p PrimitiveKind) IsFloatingPoint() bool {
	return p >= F32 && p <= F64
}

type Primitive struct {
	Kind        PrimitiveKind
	MethodTable map[string]FunctionType
}

type FunctionType struct {
	External   bool
	Name       string
	Parameters []Type
	ReturnType Type
}

type StructType struct {
	Name         string
	Members      []StructMember
	SourceModule *Module
	MethodTable  map[string]FunctionType
}

type PointerType struct {
	ElType Type
}

type BoxType struct {
	ElType Type
}

type SliceType struct {
	ElType      Type
	MethodTable map[string]FunctionType
}

type SumType struct {
	Options []Type
}

type Module struct {
	// The alias for this module's namespace. An empty string
	// represents the main module and leads to no namespacing.
	Name string

	Path       string
	Source     string
	Tokens     []token.Token
	Statements []Statement

	Imports map[string]*Module
	Exports map[string]Type
}

func (*Primitive) isType()    {}
func (*FunctionType) isType() {}
func (*StructType) isType()   {}
func (*PointerType) isType()  {}
func (*BoxType) isType()      {}
func (*SliceType) isType()    {}
func (*SumType) isType()      {}
func (*Module) isType()       {}

func (p *Primitive) Equals(t Type) bool {
	if primType, ok := t.(*Primitive); ok {
		return p.Kind == primType.Kind
	}

	return false
}

func (p *Primitive) String() string {
	return p.Kind.String()
}

func (p *Primitive) IsCopyable() bool {
	return p.Kind != Str
}

func (p *Primitive) Methods() *map[string]FunctionType {
	return &p.MethodTable
}

func (p *Primitive) IsNumeric() bool {
	return p.Kind < Str
}

func (f *FunctionType) Equals(t Type) bool {
	if funcType, ok := t.(*FunctionType); ok {
		if len(f.Parameters) != len(funcType.Parameters) {
			return false
		}

		allParamsSame := true
		for i, param := range f.Parameters {
			if !param.Equals(funcType.Parameters[i]) {
				allParamsSame = false
			}
		}

		return allParamsSame && f.ReturnType.Equals(funcType.ReturnType)
	}

	return false
}

func (f *FunctionType) String() string {
	params := ""
	for i, param := range f.Parameters {
		params += param.String()
		if i != len(f.Parameters)-1 {
			params += ", "
		}
	}
	return fmt.Sprintf("(fn (%s))", params)
}

func (f *FunctionType) IsCopyable() bool {
	// Not actually sure about this, but we could come back to it once function
	// types are actually available to the user.
	return true
}

func (f *FunctionType) Methods() *map[string]FunctionType {
	panic("FunctionType cannot have methods")
}

func (s *StructType) Equals(t Type) bool {
	if structType, ok := t.(*StructType); ok {
		return s.Name == structType.Name
	}

	return false
}

func (s *StructType) String() string {
	membersString := ""

	for i, m := range s.Members {
		membersString += m.Identifier.Lexeme + " " + m.Type.String()

		if i != len(s.Members)-1 {
			membersString += ", "
		}
	}

	return fmt.Sprintf("%s{%s}", s.Name, membersString)
}

func (s *StructType) GetMember(name string) (*StructMember, bool) {
	for _, m := range s.Members {
		if m.Identifier.Lexeme == name {
			return &m, true
		}
	}

	return nil, false
}

func (s *StructType) IsCopyable() bool {
	for _, m := range s.Members {
		if !m.Type.IsCopyable() {
			return false
		}
	}

	return true
}

func (s *StructType) Methods() *map[string]FunctionType {
	return &s.MethodTable
}

func (p *PointerType) Equals(t Type) bool {
	if ptrType, ok := t.(*PointerType); ok {
		return p.ElType.Equals(ptrType.ElType)
	}

	return false
}

func (p *PointerType) String() string {
	return fmt.Sprintf("*(%s)", p.ElType.String())
}

func (p *PointerType) IsCopyable() bool {
	return true
}

func (p *PointerType) Methods() *map[string]FunctionType {
	panic("PointerType cannot have methods")
}

func (b *BoxType) Equals(t Type) bool {
	if boxType, ok := t.(*BoxType); ok {
		return b.ElType.Equals(boxType.ElType)
	}

	return false
}

func (b *BoxType) String() string {
	return fmt.Sprintf("~%s", b.ElType.String())
}

func (b *BoxType) IsCopyable() bool {
	return false
}

func (b *BoxType) Methods() *map[string]FunctionType {
	panic("BoxType cannot have methods")
}

func (s *SliceType) Equals(t Type) bool {
	if sliceType, ok := t.(*SliceType); ok {
		return s.ElType.Equals(sliceType.ElType)
	}

	return false
}

func (s *SliceType) String() string {
	return fmt.Sprintf("[%s]", s.ElType.String())
}

func (s *SliceType) IsCopyable() bool {
	return false
}

func (s *SliceType) Methods() *map[string]FunctionType {
	return &s.MethodTable
}

func (s *SumType) Equals(t Type) bool {
	if sumType, ok := t.(*SumType); ok {
		if len(sumType.Options) != len(s.Options) {
			return false
		}

		for i, t := range sumType.Options {
			if !t.Equals(s.Options[i]) {
				return false
			}
		}

		return true
	}

	return false
}

func (s *SumType) String() string {
	result := ""
	for i, t := range s.Options {
		result += t.String()
		if i != len(s.Options)-1 {
			result += "|"
		}
	}
	return result
}

func (s *SumType) IsCopyable() bool {
	return false
}

func (s *SumType) Methods() *map[string]FunctionType {
	panic("SumType cannot have methods")
}

func (m *Module) Equals(t Type) bool {
	if modType, ok := t.(*Module); ok {
		return m.Path == modType.Path
	}

	return false
}

func (m *Module) String() string {
	return fmt.Sprintf("module{\"%s\"}", m.Path)
}

func (m *Module) IsCopyable() bool {
	return false
}

func (m *Module) Methods() *map[string]FunctionType {
	panic("SumType cannot have methods")
}

func (m *Module) TokenSourceContext(t *token.Token) string {
	source := m.Source // copy the source

	source = strings.ReplaceAll(source, "\r\n", "\n")
	sourceLines := strings.Split(source, "\n")
	numLines := len(sourceLines)

	var highlightChar byte = '^'
	offsetHighlight := make([]byte, t.Pos.Column)

	for i := 0; i < t.Pos.Column; i++ {
		if sourceLines[t.Pos.Line-1][i] == '\t' {
			offsetHighlight[i] = '\t'
		} else {
			offsetHighlight[i] = ' '
		}
	}

	offsetHighlight[t.Pos.Column-1] = highlightChar

	if t.Pos.Line == 1 {
		return fmt.Sprintf(`
%4d | %s
     | %s`,
			1,
			sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
		)
	} else if t.Pos.Line == numLines-1 {
		return fmt.Sprintf(`
%4d | %s
%4d | %s
     | %s`,
			t.Pos.Line-1, sourceLines[t.Pos.Line-2],
			t.Pos.Line, sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
		)

	} else {
		return fmt.Sprintf(`
%4d | %s
%4d | %s
     | %s
%4d | %s`,
			t.Pos.Line-1, sourceLines[t.Pos.Line-2],
			t.Pos.Line, sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
			t.Pos.Line+1, sourceLines[t.Pos.Line],
		)
	}
}

type Statement interface {
	isStatement()
}

type Parameter struct {
	Identifier token.Token
	Type       Type
}

type FunctionDeclaration struct {
	External   bool
	Exported   bool
	Identifier token.Token
	Parameters []Parameter
	ReturnType Type
	Block      *BlockStatement
}

func (f *FunctionDeclaration) ToType() FunctionType {
	var typeParams []Type
	for _, p := range f.Parameters {
		typeParams = append(typeParams, p.Type)
	}

	return FunctionType{
		External:   f.External,
		Name:       f.Identifier.Lexeme,
		Parameters: typeParams,
		ReturnType: f.ReturnType,
	}
}

type ImplBlock struct {
	Receiver Type
	Methods  []FunctionDeclaration

	ImplToken token.Token
}

type StructDeclaration struct {
	Exported   bool
	Identifier token.Token
	Members    []StructMember
}

type StructMember struct {
	Identifier token.Token
	Type       Type
}

type VariableDeclaration struct {
	Identifier token.Token
	Type       Type
	Value      Expression
}

type IfStatement struct {
	Condition        Expression
	IfBlock          BlockStatement
	ElseIfStatements []ElseIfStatement
	ElseBlock        *BlockStatement

	IfToken token.Token
}

type ElseIfStatement struct {
	Condition Expression
	Block     BlockStatement

	IfToken token.Token
}

type WhileStatement struct {
	Condition Expression
	Block     BlockStatement

	WhileToken token.Token
}

type ReturnStatement struct {
	Expression Expression

	ReturnToken token.Token
}

type ExpressionStatement struct {
	Expression Expression
}

type ImportStatement struct {
	PathToken  token.Token
	Identifier *token.Token
}

type BlockStatement struct {
	Statements []Statement
}

func (*FunctionDeclaration) isStatement() {}
func (*StructDeclaration) isStatement()   {}
func (*ImplBlock) isStatement()           {}
func (*VariableDeclaration) isStatement() {}
func (*IfStatement) isStatement()         {}
func (*WhileStatement) isStatement()      {}
func (*ReturnStatement) isStatement()     {}
func (*ExpressionStatement) isStatement() {}
func (*ImportStatement) isStatement()     {}
func (*BlockStatement) isStatement()      {}

type Expression interface {
	isExpression()
	Type() Type
	ErrorToken() token.Token
}

type UnaryExpression struct {
	Operator token.Token
	Value    Expression

	Typ Type
}

type BinaryExpression struct {
	Left     Expression
	Operator token.Token
	Right    Expression

	Typ Type
}

type VariableExpression struct {
	Identifier token.Token

	Typ Type
}

type CallExpression struct {
	Callee    Expression
	Arguments []Expression

	Typ            Type
	LeftParenToken token.Token
}

type GetExpression struct {
	Expression Expression
	Identifier token.Token

	Typ Type
}

type IndexExpression struct {
	Expression Expression
	Index      Expression

	LeftBracketToken token.Token
	Typ              Type
}

type AsExpression struct {
	Expression Expression
	TargetType Type

	AsToken token.Token
	Typ     Type
}

type IsExpression struct {
	Expression   Expression
	ComparedType Type

	IsToken token.Token
	Typ     Type
}

type CompositeLiteral struct {
	Typ                 Type
	NamedInitializers   *[]NamedInitializer
	UnnamedInitializers *[]Expression

	LeftBraceToken token.Token
}

type SliceLiteral struct {
	Expressions []Expression

	Typ              Type
	LeftBracketToken token.Token
}

type NamedInitializer struct {
	Identifier token.Token
	Value      Expression
}

type ReferenceOf struct {
	Target Expression

	Typ      Type
	AndToken token.Token
}

type Dereference struct {
	Expression Expression

	Typ        Type
	CaretToken token.Token
}

type Literal struct {
	Token        token.Token
	LiteralValue string

	Typ Type
}

func (*UnaryExpression) isExpression()    {}
func (*BinaryExpression) isExpression()   {}
func (*VariableExpression) isExpression() {}
func (*CallExpression) isExpression()     {}
func (*GetExpression) isExpression()      {}
func (*IndexExpression) isExpression()    {}
func (*AsExpression) isExpression()       {}
func (*IsExpression) isExpression()       {}
func (*CompositeLiteral) isExpression()   {}
func (*SliceLiteral) isExpression()       {}
func (*ReferenceOf) isExpression()        {}
func (*Dereference) isExpression()        {}
func (*Literal) isExpression()            {}

func (u *UnaryExpression) Type() Type {
	return u.Typ
}

func (u *UnaryExpression) ErrorToken() token.Token {
	return u.Operator
}

func (b *BinaryExpression) Type() Type {
	return b.Typ
}

func (b *BinaryExpression) ErrorToken() token.Token {
	return b.Operator
}

func (v *VariableExpression) Type() Type {
	return v.Typ
}

func (v *VariableExpression) ErrorToken() token.Token {
	return v.Identifier
}

func (c *CallExpression) Type() Type {
	return c.Typ
}

func (c *CallExpression) ErrorToken() token.Token {
	return c.LeftParenToken
}

func (g *GetExpression) Type() Type {
	return g.Typ
}

func (g *GetExpression) ErrorToken() token.Token {
	return g.Identifier
}

func (i *IndexExpression) Type() Type {
	return i.Typ
}

func (i *IndexExpression) ErrorToken() token.Token {
	return i.LeftBracketToken
}

func (a *AsExpression) Type() Type {
	return a.Typ
}

func (a *AsExpression) ErrorToken() token.Token {
	return a.AsToken
}

func (i *IsExpression) Type() Type {
	return i.Typ
}

func (i *IsExpression) ErrorToken() token.Token {
	return i.IsToken
}

func (c *CompositeLiteral) Type() Type {
	return c.Typ
}

func (c *CompositeLiteral) ErrorToken() token.Token {
	return c.LeftBraceToken
}

func (s *SliceLiteral) Type() Type {
	return s.Typ
}

func (s *SliceLiteral) ErrorToken() token.Token {
	return s.LeftBracketToken
}

func (r *ReferenceOf) Type() Type {
	return r.Typ
}

func (r *ReferenceOf) ErrorToken() token.Token {
	return r.AndToken
}

func (r *Dereference) Type() Type {
	return r.Typ
}

func (d *Dereference) ErrorToken() token.Token {
	return d.CaretToken
}

func (l *Literal) Type() Type {
	return l.Typ
}

func (l *Literal) ErrorToken() token.Token {
	return l.Token
}
