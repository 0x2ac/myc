package ast

import (
	"fmt"

	"github.com/kartiknair/myc/lexer"
)

type Type interface {
	isType()
	Equals(Type) bool
	String() string
}

type Primitive struct {
	Name string
}

type FunctionType struct {
	Parameters []Type
	ReturnType Type
}

type StructType struct {
	Name    string
	Members []StructMember
}

func (*Primitive) isType()    {}
func (*FunctionType) isType() {}
func (*StructType) isType()   {}

func (p *Primitive) Equals(t Type) bool {
	if primType, ok := t.(*Primitive); ok {
		return p.Name == primType.Name
	}

	return false
}

func (p *Primitive) String() string {
	return p.Name
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

type Statement interface {
	isStatement()
}

type Parameter struct {
	Identifier lexer.Token
	Type       Type
}

type FunctionDeclaration struct {
	Identifier lexer.Token
	Parameters []Parameter
	ReturnType Type
	Block      BlockStatement
}

type StructDeclaration struct {
	Identifier lexer.Token
	Members    []StructMember
}

type StructMember struct {
	Identifier lexer.Token
	Type       Type
}

type VariableDeclaration struct {
	Identifier lexer.Token
	Type       Type
	Value      Expression
}

type ConstantDeclaration struct {
	Identifier lexer.Token
	Value      Expression
}

type PrintStatement struct {
	Expressions []Expression
}

type ReturnStatement struct {
	Expression Expression

	ReturnToken lexer.Token
}

type ExpressionStatement struct {
	Expression Expression
}

type BlockStatement struct {
	Statements []Statement
}

func (*FunctionDeclaration) isStatement() {}
func (*StructDeclaration) isStatement()   {}
func (*VariableDeclaration) isStatement() {}
func (*ConstantDeclaration) isStatement() {}
func (*PrintStatement) isStatement()      {}
func (*ReturnStatement) isStatement()     {}
func (*ExpressionStatement) isStatement() {}
func (*BlockStatement) isStatement()      {}

type Expression interface {
	isExpression()
	Type() Type
}

type UnaryExpression struct {
	Operator lexer.Token
	Value    Expression

	Typ Type
}

type BinaryExpression struct {
	Left     Expression
	Operator lexer.Token
	Right    Expression

	Typ Type
}

type VariableExpression struct {
	Identifier lexer.Token

	Typ Type
}

type CallExpression struct {
	Callee    Expression
	Arguments []Expression

	Typ            Type
	LeftParenToken lexer.Token
}

type GetExpression struct {
	Expression Expression
	Identifier lexer.Token

	Typ Type
}

type CompositeLiteral struct {
	Typ                 Type
	NamedInitializers   *[]NamedInitializer
	UnnamedInitializers *[]Expression

	LeftBraceToken lexer.Token
}

type NamedInitializer struct {
	Identifier lexer.Token
	Value      Expression
}

type Literal struct {
	LiteralType  lexer.TokenType
	LiteralValue string

	Typ Type
}

func (*UnaryExpression) isExpression()    {}
func (*BinaryExpression) isExpression()   {}
func (*VariableExpression) isExpression() {}
func (*CallExpression) isExpression()     {}
func (*GetExpression) isExpression()      {}
func (*CompositeLiteral) isExpression()   {}
func (*Literal) isExpression()            {}

func (u *UnaryExpression) Type() Type {
	return u.Typ
}
func (b *BinaryExpression) Type() Type {
	return b.Typ
}
func (v *VariableExpression) Type() Type {
	return v.Typ
}
func (c *CallExpression) Type() Type {
	return c.Typ
}
func (g *GetExpression) Type() Type {
	return g.Typ
}
func (c *CompositeLiteral) Type() Type {
	return c.Typ
}
func (l *Literal) Type() Type {
	return l.Typ
}
