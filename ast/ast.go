package ast

import "github.com/kartiknair/myc/lexer"

type Type interface {
	isType()
	Equals(Type) bool
}

type Primitive struct {
	Name string
}

func (*Primitive) isType() {}

func (self *Primitive) Equals(t Type) bool {
	if primitiveT, ok := t.(*Primitive); ok {
		return self.Name == primitiveT.Name
	}

	return false
}

type Statement interface {
	isStatement()
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

type ExpressionStatement struct {
	Expression Expression
}

type BlockStatement struct {
	Statements []Statement
}

func (*VariableDeclaration) isStatement() {}
func (*ConstantDeclaration) isStatement() {}
func (*PrintStatement) isStatement()      {}
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

type Literal struct {
	LiteralType  lexer.TokenType
	LiteralValue string

	Typ Type
}

func (*UnaryExpression) isExpression()    {}
func (*BinaryExpression) isExpression()   {}
func (*VariableExpression) isExpression() {}
func (*Literal) isExpression()            {}

func (self *UnaryExpression) Type() Type {
	return self.Typ
}
func (self *BinaryExpression) Type() Type {
	return self.Typ
}
func (self *VariableExpression) Type() Type {
	return self.Typ
}
func (self *Literal) Type() Type {
	return self.Typ
}
