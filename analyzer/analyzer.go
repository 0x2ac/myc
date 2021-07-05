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

func (s *SymbolTable) shadow(name string, typ ast.Type) {
	s.values[name] = typ
}

var namespace = NewSymbolTable()
var typespace = NewSymbolTable()

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

func resolveStructType(st *ast.StructType) error {
	t, err := typespace.get(st.Name)

	if err != nil {
		return errors.New(fmt.Sprintf("Could not resolve struct type: '%s'", st.Name))
	}

	*st = *t.(*ast.StructType)

	for _, m := range st.Members {
		if _, ok := m.Type.(*ast.StructType); ok {
			err = resolveStructType(m.Type.(*ast.StructType))
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func analyzeExpression(expr ast.Expression) {
	switch expr.(type) {
	case *ast.UnaryExpression:
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
	case *ast.BinaryExpression:
		e := expr.(*ast.BinaryExpression)
		analyzeExpression(e.Left)
		analyzeExpression(e.Right)

		if !e.Left.Type().Equals(e.Right.Type()) {
			analysisError(e.Operator, "Binary expressions must have the same type on both sides.")
		}

		prim, ok := e.Left.Type().(*ast.Primitive)
		if !ok {
			analysisError(e.Operator, fmt.Sprintf(
				"Operator: '%s' can only be used on primitive types.",
				e.Operator.Lexeme,
			))
		}

		if e.Operator.Type == lexer.EQUAL {
			e.Typ = e.Left.Type()
		} else if e.Operator.Type == lexer.PLUS ||
			e.Operator.Type == lexer.MINUS ||
			e.Operator.Type == lexer.STAR ||
			e.Operator.Type == lexer.SLASH {
			// Arithmetic operators are only for numeric expressions
			if !prim.IsNumeric() {
				analysisError(e.Operator, fmt.Sprintf(
					"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`).",
					e.Operator.Lexeme,
				))
			}

			e.Typ = e.Left.Type()
		} else if e.Operator.Type == lexer.LESSER ||
			e.Operator.Type == lexer.GREATER ||
			e.Operator.Type == lexer.LESSER_EQUAL ||
			e.Operator.Type == lexer.GREATER_EQUAL {
			// Comparison operators are also for numeric expressions
			if !prim.IsNumeric() {
				analysisError(e.Operator, fmt.Sprintf(
					"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`).",
					e.Operator.Lexeme,
				))
			}

			e.Typ = &ast.Primitive{Name: "bool"}
		} else if e.Operator.Type == lexer.EQUAL_EQUAL ||
			e.Operator.Type == lexer.BANG_EQUAL {
			e.Typ = &ast.Primitive{Name: "bool"}
		} else if e.Operator.Type == lexer.AND_AND || e.Operator.Type == lexer.OR_OR {
			// Logical operators only work on `bool` expressions
			if prim.Name != "bool" {
				analysisError(e.Operator, fmt.Sprintf(
					"Operator: '%s' can only be used on values of type `bool`.",
					e.Operator.Lexeme,
				))
			}

			e.Typ = &ast.Primitive{Name: "bool"}

		}
	case *ast.CallExpression:
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
	case *ast.VariableExpression:
		e := expr.(*ast.VariableExpression)
		valueType, err := namespace.get(e.Identifier.Lexeme)
		if err != nil {
			analysisError(e.Identifier, err.Error())
		}
		e.Typ = valueType
	case *ast.GetExpression:
		e := expr.(*ast.GetExpression)

		analyzeExpression(e.Expression)

		structType, ok := e.Expression.Type().(*ast.StructType)
		// e.g. 32.value, "hello".foo, etc.
		if !ok {
			analysisError(
				e.Identifier,
				fmt.Sprintf(
					"Expected structure type, instead got type: '%s'.",
					e.Expression.Type().String(),
				),
			)
		}

		err := resolveStructType(structType)
		if err != nil {
			analysisError(e.Identifier, "Internal compiler error:"+err.Error())
		}

		foundMember, ok := structType.GetMember(e.Identifier.Lexeme)
		if !ok {
			analysisError(
				e.Identifier,
				fmt.Sprintf(
					"Type: '%s' does not have field: '%s'.",
					structType.Name,
					e.Identifier.Lexeme,
				),
			)
		}

		e.Typ = foundMember.Type
	case *ast.CompositeLiteral:
		e := expr.(*ast.CompositeLiteral)
		r := e.Typ.(*ast.StructType)
		err := resolveStructType(r)

		if err != nil {
			analysisError(e.LeftBraceToken, err.Error())
		}

		if e.NamedInitializers == nil && e.UnnamedInitializers == nil {
			analysisError(
				e.LeftBraceToken,
				"Missing intializers for members in composite literal.",
			)
		} else if e.NamedInitializers != nil {
			if len(*e.NamedInitializers) != len(r.Members) {
				analysisError(
					e.LeftBraceToken,
					fmt.Sprintf(
						"Number of named intitializers does not match. Expected: %d for type '%s', instead got: %d.",
						len(r.Members),
						r.Name,
						len(*e.NamedInitializers),
					),
				)
			}

			for _, initializer := range *e.NamedInitializers {
				analyzeExpression(initializer.Value)

				resolvedMember, ok := r.GetMember(initializer.Identifier.Lexeme)

				// Check if the resolved type has a field with that name
				if !ok {
					analysisError(
						initializer.Identifier,
						fmt.Sprintf(
							"Type: '%s' does not have a field with name: '%s'.",
							r.Name,
							initializer.Identifier.Lexeme,
						),
					)
				}

				// Check if the expression has the appropriate type for the member
				if !initializer.Value.Type().Equals(resolvedMember.Type) {
					analysisError(
						initializer.Identifier,
						fmt.Sprintf(
							"`%s.%s` has type: '%s', got expression of type: '%s'.",
							r.Name,
							initializer.Identifier.Lexeme,
							resolvedMember.Type.String(),
							initializer.Value.Type().String(),
						),
					)
				}
			}
		} else if e.UnnamedInitializers != nil {
			if len(*e.UnnamedInitializers) != len(r.Members) {
				analysisError(
					e.LeftBraceToken,
					fmt.Sprintf(
						"Number of anonymous intitializers does not match. Expected: %d for type '%s', instead got: %d.",
						len(r.Members),
						r.Name,
						len(*e.UnnamedInitializers),
					),
				)
			}

			for i, intializer := range *e.UnnamedInitializers {
				analyzeExpression(intializer)

				if !r.Members[i].Type.Equals(intializer.Type()) {
					analysisError(
						e.LeftBraceToken,
						fmt.Sprintf(
							"Positional initializer has incorrect type. Expected value of type: '%s' instead got value of type: '%s'.",
							r.Members[i].Type.String(),
							intializer.Type().String(),
						),
					)
				}
			}
		}

		e.Typ = r
	case *ast.ReferenceOf:
		e := expr.(*ast.ReferenceOf)
		analyzeExpression(e.Target)
		e.Typ = &ast.PointerType{ElType: e.Target.Type()}
	case *ast.Dereference:
		e := expr.(*ast.Dereference)
		analyzeExpression(e.Expression)

		ptrType, ok := e.Expression.Type().(*ast.PointerType)
		if !ok {
			analysisError(
				e.StarToken,
				fmt.Sprintf(
					"Dereference of non-pointer type: '%s'",
					e.Expression.Type().String(),
				),
			)
		}

		e.Typ = ptrType.ElType
	case *ast.Literal:
		e := expr.(*ast.Literal)
		switch e.LiteralType {
		case lexer.INT:
			e.Typ = &ast.Primitive{Name: "int"}
		case lexer.FLOAT:
			e.Typ = &ast.Primitive{Name: "float"}
		case lexer.TRUE:
			fallthrough
		case lexer.FALSE:
			e.Typ = &ast.Primitive{Name: "bool"}
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
			namespace.shadow(param.Identifier.Lexeme, param.Type)
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
	case *ast.StructDeclaration:
		s := statement.(*ast.StructDeclaration)

		for _, m := range s.Members {
			if m.Type.Equals(&ast.StructType{Name: s.Identifier.Lexeme}) {
				analysisError(m.Identifier, "Cannot nest struct type as size would be unknown.")
			}
		}

		err := typespace.declare(
			s.Identifier.Lexeme,
			&ast.StructType{
				Name:    s.Identifier.Lexeme,
				Members: s.Members,
			},
		)
		if err != nil {
			analysisError(s.Identifier, fmt.Sprintf("Type name '%s' is already in use.", s.Identifier.Lexeme))
		}
	case *ast.VariableDeclaration:
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
	case *ast.ConstantDeclaration:
		s := statement.(*ast.ConstantDeclaration)
		analyzeExpression(s.Value)
		err := namespace.declare(s.Identifier.Lexeme, s.Value.Type())
		if err != nil {
			analysisError(s.Identifier, err.Error())
		}
	case *ast.IfStatement:
		s := statement.(*ast.IfStatement)
		analyzeExpression(s.Condition)
		if p, ok := s.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
			analysisError(
				s.IfToken,
				fmt.Sprintf(
					"If statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
					s.Condition.Type().String(),
				),
			)
		}
		analyzeStatement(&s.IfBlock)

		if len(s.ElseIfStatements) != 0 {
			for _, e := range s.ElseIfStatements {
				analyzeExpression(e.Condition)
				if p, ok := e.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
					analysisError(
						e.IfToken,
						fmt.Sprintf(
							"Else if statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
							e.Condition.Type().String(),
						),
					)
				}
				analyzeStatement(&e.Block)
			}
		}

		if s.ElseBlock != nil {
			analyzeStatement(s.ElseBlock)
		}
	case *ast.WhileStatement:
		s := statement.(*ast.WhileStatement)
		analyzeExpression(s.Condition)
		if p, ok := s.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
			analysisError(
				s.WhileToken,
				fmt.Sprintf(
					"While statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
					s.Condition.Type().String(),
				),
			)
		}
		analyzeStatement(&s.Block)
	case *ast.PrintStatement:
		s := statement.(*ast.PrintStatement)
		for _, expr := range s.Expressions {
			analyzeExpression(expr)
		}
	case *ast.ReturnStatement:
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
	case *ast.ExpressionStatement:
		s := statement.(*ast.ExpressionStatement)
		analyzeExpression(s.Expression)
	case *ast.BlockStatement:
		oldScope := namespace // copy the old scope
		namespace = NewSymbolTableFromEnclosing(namespace)

		s := statement.(*ast.BlockStatement)
		for _, stmt := range s.Statements {
			analyzeStatement(stmt)
		}

		namespace = oldScope
	}
}
