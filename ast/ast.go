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

func (*Primitive) isType()    {}
func (*FunctionType) isType() {}

func (self *Primitive) Equals(t Type) bool {
	if primitiveT, ok := t.(*Primitive); ok {
		return self.Name == primitiveT.Name
	}

	return false
}

func (self *Primitive) String() string {
	return self.Name
}

func (self *FunctionType) Equals(t Type) bool {
	if funcType, ok := t.(*FunctionType); ok {
		if len(self.Parameters) != len(funcType.Parameters) {
			return false
		}

		allParamsSame := true
		for i, param := range self.Parameters {
			if !param.Equals(funcType.Parameters[i]) {
				allParamsSame = false
			}
		}

		return allParamsSame && self.ReturnType.Equals(funcType.ReturnType)
	}

	return false
}

func (self *FunctionType) String() string {
	params := ""
	for i, param := range self.Parameters {
		params += param.String()
		if i != len(self.Parameters)-1 {
			params += ", "
		}
	}
	return fmt.Sprintf("(fn (%s))", params)
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

type Literal struct {
	LiteralType  lexer.TokenType
	LiteralValue string

	Typ Type
}

func (*UnaryExpression) isExpression()    {}
func (*BinaryExpression) isExpression()   {}
func (*VariableExpression) isExpression() {}
func (*CallExpression) isExpression()     {}
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
func (self *CallExpression) Type() Type {
	return self.Typ
}
func (self *Literal) Type() Type {
	return self.Typ
}
