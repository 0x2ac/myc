package analyzer

import (
	"errors"
	"log"

	"github.com/kartiknair/myc/ast"
	"github.com/kartiknair/myc/lexer"
)

func analysisError(token lexer.Token, message string) {
	log.Fatalf(
		"[Analaysis Error: %d:%d] %s",
		token.Pos.Line, token.Pos.Column, message,
	)
}

type SymbolTable struct {
	values    map[string]ast.Type
	enclosing *SymbolTable
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		values:    make(map[string]ast.Type),
		enclosing: nil,
	}
}

func NewSymbolTableFromEnclosing(enclosing *SymbolTable) *SymbolTable {
	return &SymbolTable{
		values:    make(map[string]ast.Type),
		enclosing: enclosing,
	}
}

func (s *SymbolTable) get(name string) (ast.Type, error) {
	if value, ok := s.values[name]; ok {
		return value, nil
	} else if s.enclosing == nil {
		return nil, errors.New("Value with that name is not declared.")
	}

	return s.enclosing.get(name)
}

func (s *SymbolTable) declare(name string, initialType ast.Type) error {
	if _, err := s.get(name); err == nil {
		return errors.New("Redeclaration of variable.")
	}

	s.values[name] = initialType
	return nil
}

var namespace = NewSymbolTable()

// Modifies the `statements` argument to add and check type information.
// Panics with a useful error message if program has semantic errors.
//
// Note that we take the statements slice by value since we will not be
// modifying the slice header and only need to modify the elements (and
// therefore the underying buffer).
func Analyze(statements []ast.Statement) {
	for _, statement := range statements {
		analyzeStatement(statement)
	}
}

func analyzeExpression(expr ast.Expression) {
	switch expr.(type) {
	case *ast.UnaryExpression:
		{
			e := expr.(*ast.UnaryExpression)
			analyzeExpression(e.Value)
			if _, ok := e.Value.Type().(*ast.Primitive); !ok {
				// ^ translation: if value's type is not a primitive
				analysisError(e.Operator, "Unary operator can only be on primitive types.")
			}

			prim := e.Value.Type().(*ast.Primitive)
			if prim.Name != "int" && prim.Name != "float" {
				analysisError(e.Operator, "Unary operator can only be on `int` or `float` types.")
			}

			e.Typ = prim
		}
	case *ast.BinaryExpression:
		{
			e := expr.(*ast.BinaryExpression)
			analyzeExpression(e.Left)
			analyzeExpression(e.Right)
			if !e.Left.Type().Equals(e.Right.Type()) {
				analysisError(e.Operator, "Binary expressions must have the same type on both sides.")
			}

			e.Typ = e.Left.Type()
		}
	case *ast.VariableExpression:
		{
			e := expr.(*ast.VariableExpression)
			valueType, err := namespace.get(e.Identifier.Lexeme)
			if err != nil {
				analysisError(e.Identifier, err.Error())
			}
			e.Typ = valueType
		}
	case *ast.Literal:
		{
			e := expr.(*ast.Literal)
			switch e.LiteralType {
			case lexer.INT:
				{
					e.Typ = &ast.Primitive{Name: "int"}
				}
			case lexer.FLOAT:
				{
					e.Typ = &ast.Primitive{Name: "float"}
				}
			}
		}
	}
}

func analyzeStatement(statement ast.Statement) {
	switch statement.(type) {
	case *ast.VariableDeclaration:
		{
			s := statement.(*ast.VariableDeclaration)

			if s.Value != nil {
				analyzeExpression(s.Value)
			}

			if s.Type == nil && s.Value == nil {
				// e.g. var foo
				analysisError(s.Identifier, "Variable declaration must have either type or initial value.")
			} else if s.Type == nil && s.Value != nil {
				// e.g. var foo = 42
				s.Type = s.Value.Type()
			} else if s.Type != nil && s.Value != nil {
				// e.g. var foo string = 42
				if !s.Type.Equals(s.Value.Type()) {
					analysisError(s.Identifier, "Initial expression has different type than provided one.")
				}
			}
			// We do nothing in the final case:
			// e.g. var foo int

			err := namespace.declare(s.Identifier.Lexeme, s.Type)
			if err != nil {
				analysisError(s.Identifier, err.Error())
			}
		}
	case *ast.ConstantDeclaration:
		{
			s := statement.(*ast.ConstantDeclaration)
			analyzeExpression(s.Value)
			err := namespace.declare(s.Identifier.Lexeme, s.Value.Type())
			if err != nil {
				analysisError(s.Identifier, err.Error())
			}
		}
	case *ast.PrintStatement:
		{
			s := statement.(*ast.PrintStatement)
			for _, expr := range s.Expressions {
				analyzeExpression(expr)
			}
		}
	case *ast.ExpressionStatement:
		{
			s := statement.(*ast.ExpressionStatement)
			analyzeExpression(s.Expression)
		}
	case *ast.BlockStatement:
		{
			oldScope := namespace // copy the old scope
			namespace = NewSymbolTableFromEnclosing(namespace)

			s := statement.(*ast.BlockStatement)
			for _, stmt := range s.Statements {
				analyzeStatement(stmt)
			}

			namespace = oldScope
		}
	}
}
