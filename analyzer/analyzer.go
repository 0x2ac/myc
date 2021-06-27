package analyzer

import (
	"errors"
	"fmt"
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
				// The value is not a primitive.
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
	case *ast.CallExpression:
		{
			e := expr.(*ast.CallExpression)
			analyzeExpression(e.Callee)

			if _, ok := e.Callee.Type().(*ast.FunctionType); !ok {
				// The callee is not a function.
				analysisError(
					e.LeftParenToken,
					fmt.Sprintf(
						"Attempting to call value of non-functional type: `%s`",
						e.Callee.Type().String(),
					),
				)
			}

			signature := e.Callee.Type().(*ast.FunctionType)

			if len(e.Arguments) != len(signature.Parameters) {
				analysisError(
					e.LeftParenToken,
					fmt.Sprintf(
						"Function being called requires %d parameters. Received %d arguments instead.",
						len(signature.Parameters),
						len(e.Arguments),
					),
				)
			}

			for i, param := range signature.Parameters {
				analyzeExpression(e.Arguments[i])

				if !param.Equals(e.Arguments[i].Type()) {
					analysisError(
						e.LeftParenToken,
						fmt.Sprintf(
							"Mismatched type for positional argument %d. Expected value of type: `%s`, instead got value of type: `%s`.",
							i+1,
							param.String(),
							e.Arguments[i].Type().String(),
						),
					)
				}
			}

			e.Typ = signature.ReturnType
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

// Global used to track what expressions are valid return values.
var functionScope *ast.FunctionDeclaration

// Tracks whether the current function has a valid return statement.
var hasValidReturn bool

func analyzeStatement(statement ast.Statement) {
	switch statement.(type) {
	case *ast.FunctionDeclaration:
		{
			oldFunctionScope := functionScope

			s := statement.(*ast.FunctionDeclaration)
			functionScope = s

			onlyTypeParams := []ast.Type{}
			for _, p := range s.Parameters {
				onlyTypeParams = append(onlyTypeParams, p.Type)
			}

			signature := ast.FunctionType{
				Parameters: onlyTypeParams,
				ReturnType: s.ReturnType,
			}

			err := namespace.declare(s.Identifier.Lexeme, &signature)
			if err != nil {
				analysisError(s.Identifier, err.Error())
			}

			oldScope := namespace // copy the old scope
			namespace = NewSymbolTableFromEnclosing(namespace)

			for _, param := range s.Parameters {
				namespace.declare(param.Identifier.Lexeme, param.Type)
			}

			for _, statement := range s.Block.Statements {
				analyzeStatement(statement)
			}

			namespace = oldScope
			functionScope = oldFunctionScope

			if !hasValidReturn && s.ReturnType != nil {
				analysisError(
					s.Identifier,
					fmt.Sprintf(
						"Function does not have a return statement. Expected return of type: `%s`.",
						s.ReturnType,
					),
				)
			}
		}
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
	case *ast.ReturnStatement:
		{
			s := statement.(*ast.ReturnStatement)

			if s.Expression != nil {
				analyzeExpression(s.Expression)
			}

			if functionScope == nil {
				analysisError(s.ReturnToken, "Return statement not inside a function.")
			} else if s.Expression == nil && functionScope.ReturnType != nil {
				analysisError(
					s.ReturnToken,
					fmt.Sprintf(
						"Missing expression in return statement. Function has return type: `%s`.",
						functionScope.ReturnType.String(),
					),
				)
			} else if s.Expression != nil && functionScope.ReturnType == nil {
				analysisError(s.ReturnToken, "Returning value in void function.")
			} else if s.Expression != nil {
				if !s.Expression.Type().Equals(functionScope.ReturnType) {
					analysisError(
						s.ReturnToken,
						fmt.Sprintf(
							"Mismatched return type. Expected type: `%s` instead got: `%s`.",
							functionScope.ReturnType.String(),
							s.Expression.Type().String(),
						),
					)
				}
			}

			hasValidReturn = true
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
