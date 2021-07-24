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
		"%s\nanalysis-error: %d:%d: %s",
		token.SourceContext(),
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

func isLvalue(e ast.Expression) bool {
	if _, ok := e.(*ast.VariableExpression); ok {
		return true
	} else if _, ok := e.(*ast.GetExpression); ok {
		return true
	}

	return false
}

// Returns whether an expression (e) is assignable to a variable of given type (t).
// Will return an error in case of a mismatch.
func isAssignable(valueType ast.Type, targetType ast.Type, valueIsLvalue bool, options ...bool) error {
	allowLvalueAssignmentToNoCopyTypes := false

	if len(options) == 0 {
	} else if len(options) == 1 {
		allowLvalueAssignmentToNoCopyTypes = options[0]
	} else {
		panic("Invalid call to `isAssignable`")
	}

	if !valueType.Equals(targetType) {
		targetSumT, targetIsSumType := targetType.(*ast.SumType)
		valueSumT, valueIsSumType := valueType.(*ast.SumType)

		if p, ok := targetType.(*ast.PointerType); ok {
			if _, ok := valueType.(*ast.PointerType); !ok {
				return errors.New(fmt.Sprintf(
					"Value of type: '%s' cannot be used for type: '%s'.",
					valueType.String(), targetType.String(),
				))
			}

			err := isAssignable(valueType.(*ast.PointerType).ElType, p.ElType, valueIsLvalue, options...)
			if err != nil {
				return errors.New(fmt.Sprintf(
					"Mismatched pointer types: cannot assign value of type: '%s' to type: '%s'.",
					valueType.String(), targetType.String(),
				))
			}
		} else if b, ok := targetType.(*ast.BoxType); ok {
			// Boxes can have unboxed variables assigned to them:
			if !b.ElType.Equals(valueType) {
				return errors.New(fmt.Sprintf(
					"Mismatched box types. Cannot assign value of type: '%s' to type: '%s'.",
					valueType.String(), targetType.String(),
				))
			}
		} else if targetIsSumType && !valueIsSumType {
			// Sum types can have any of their options assigned to them
			eTypeInOptions := false
			for _, o := range targetSumT.Options {
				if o.Equals(valueType) && !eTypeInOptions {
					eTypeInOptions = true
				}
			}

			if !eTypeInOptions {
				return errors.New(fmt.Sprintf(
					"Expression of type: '%s' is not an option for sum type: '%s'",
					valueType.String(), targetSumT.String(),
				))
			}
		} else if valueIsSumType && !targetIsSumType {
			return errors.New(fmt.Sprintf(
				"Cannot implicitly cast sum-type: '%s' to its option: '%s'.\n  help: cast it explicitly using `as`.",
				valueSumT.String(), targetType.String(),
			))
		} else if valueIsSumType && targetIsSumType {
			// Code to check for this case can be found in git history.
			// Making it an error for now to make implementation simpler.
			return errors.New(fmt.Sprintf(
				"Cannot assign sum-type: '%s' to sum-type: '%s'.",
				valueSumT.String(), targetSumT.String(),
			))
		} else {
			return errors.New(fmt.Sprintf(
				"Cannot assign value of type: '%s' to type: '%s'.",
				valueType.String(), targetType.String(),
			))
		}
	}

	if !targetType.IsCopyable() && !allowLvalueAssignmentToNoCopyTypes {
		// If `t` is not copyable it can only be assigned rvalues
		if valueIsLvalue {
			return errors.New(fmt.Sprintf(
				"Cannot assign lvalue to type: '%s'.",
				targetType.String(),
			))
		}
	}

	return nil
}

// Modifies the `statements` argument to add and check type information.
// Panics with a useful error message if program has semantic errors.
//
// Note that we take the statements slice by value since we will not be
// modifying the slice header and only need to modify the elements (and
// therefore the underying buffer).
func Analyze(statements []ast.Statement) {
	for _, statement := range statements {
		analyzeStatement(statement)

		if f, ok := statement.(*ast.FunctionDeclaration); ok {
			if f.Identifier.Lexeme == "main" {
				// Special checking for top-level main function
				if f.ReturnType != nil {
					analysisError(f.Identifier, "Top level main function must not return anything.")
				} else if len(f.Parameters) != 0 {
					analysisError(f.Identifier, "Top level main function cannot take any parameters.")
				}
			}
		}
	}
}

func resolveStructType(st *ast.StructType) {
	t, err := typespace.get(st.Name)

	if err != nil {
		// TODO: make this a reported error instead!
		panic(fmt.Sprintf("Could not resolve struct type: '%s'", st.Name))
	}

	*st = *t.(*ast.StructType)

	for _, m := range st.Members {
		if _, ok := m.Type.(*ast.StructType); ok {
			resolveStructType(m.Type.(*ast.StructType))
		}
	}
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

		if e.Operator.Type == lexer.EQUAL {
			_, isVariableExpr := e.Left.(*ast.VariableExpression)
			_, isGetExpr := e.Left.(*ast.GetExpression)
			_, isDeref := e.Left.(*ast.Dereference)

			targetIsLvalue := isVariableExpr || isGetExpr || isDeref

			if !targetIsLvalue {
				analysisError(e.Operator, "Target for assignment is not lvalue.")
			}

			err := isAssignable(e.Right.Type(), e.Left.Type(), isLvalue(e.Right))
			if err != nil {
				analysisError(e.Operator, err.Error())
			}
		} else if e.Operator.Type != lexer.EQUAL {
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

			if e.Operator.Type == lexer.PLUS {
				if !prim.IsNumeric() && prim.Name != "str" {
					analysisError(e.Operator, fmt.Sprintf(
						"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`) and `str`s.",
						e.Operator.Lexeme,
					))
				}

				e.Typ = e.Left.Type()
			} else if e.Operator.Type == lexer.MINUS ||
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

			allowLvalueAssignment := true

			err := isAssignable(e.Arguments[i].Type(), param, isLvalue(e.Arguments[i]), allowLvalueAssignment)

			if err != nil {
				analysisError(
					e.LeftParenToken,
					"Mismatched type for positional argument. "+err.Error(),
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

		resolveStructType(structType)

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
	case *ast.IndexExpression:
		e := expr.(*ast.IndexExpression)
		analyzeExpression(e.Expression)
		analyzeExpression(e.Index)

		st, exprIsSlice := e.Expression.Type().(*ast.SliceType)
		prim, indexIsPrimitive := e.Index.Type().(*ast.Primitive)

		if !exprIsSlice {
			analysisError(
				e.LeftBracketToken,
				fmt.Sprintf(
					"Index operation can only be done on slice type values. Received value of type: '%s' instead",
					e.Expression.Type().String(),
				),
			)
		} else if !indexIsPrimitive || prim.Name != "int" {
			analysisError(
				e.LeftBracketToken,
				fmt.Sprintf(
					"Index can only of type `int`. Attempting to index with value of type: '%s' instead",
					e.Index.Type().String(),
				),
			)
		}

		e.Typ = st.ElType
	case *ast.AsExpression:
		e := expr.(*ast.AsExpression)

		analyzeExpression(e.Expression)
		if sumType, ok := e.Expression.Type().(*ast.SumType); !ok {
			analysisError(e.AsToken, "Can only type cast sum-type expressions.")
		} else {
			foundOption := false
			for _, o := range sumType.Options {
				if o.Equals(e.TargetType) {
					foundOption = true
					break
				}
			}

			if !foundOption {
				analysisError(e.AsToken, fmt.Sprintf(
					"Cannot cast expression of sum-type: '%s' to type: '%s'.",
					sumType.String(), e.TargetType.String(),
				))
			}
		}

		e.Typ = e.TargetType
	case *ast.IsExpression:
		e := expr.(*ast.IsExpression)

		analyzeExpression(e.Expression)
		if sumType, ok := e.Expression.Type().(*ast.SumType); !ok {
			analysisError(e.IsToken, "Can only type check sum-type expressions.")
		} else {
			foundOption := false
			for _, o := range sumType.Options {
				if o.Equals(e.ComparedType) {
					foundOption = true
					break
				}
			}

			if !foundOption {
				analysisError(e.IsToken, fmt.Sprintf(
					"Unnecessary type-check, sum-type: '%s' can never be type: '%s'.",
					sumType.String(), e.ComparedType.String(),
				))
			}
		}

		e.Typ = &ast.Primitive{Name: "bool"}
	case *ast.CompositeLiteral:
		e := expr.(*ast.CompositeLiteral)
		r := e.Typ.(*ast.StructType)
		resolveStructType(r)

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
				err := isAssignable(initializer.Value.Type(), resolvedMember.Type, isLvalue(initializer.Value))

				if err != nil {
					analysisError(
						initializer.Identifier,
						"Mismatched type for struct field. "+err.Error(),
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

			for i, initializer := range *e.UnnamedInitializers {
				analyzeExpression(initializer)

				err := isAssignable(initializer.Type(), r.Members[i].Type, isLvalue(initializer))

				if err != nil {
					analysisError(
						e.LeftBraceToken,
						"Positional initializer has incorrect type. "+err.Error(),
					)
				}
			}
		}

		e.Typ = r
	case *ast.SliceLiteral:
		e := expr.(*ast.SliceLiteral)

		var elType ast.Type
		for _, initializer := range e.Expressions {
			analyzeExpression(initializer)
			if elType == nil {
				elType = initializer.Type()
			} else {
				err := isAssignable(initializer.Type(), elType, isLvalue(initializer))
				if err != nil {
					if s, ok := elType.(*ast.SumType); ok {
						s.Options = append(s.Options, initializer.Type())
					} else {
						elType = &ast.SumType{Options: []ast.Type{elType, initializer.Type()}}
					}
				}
			}
		}

		e.Typ = &ast.SliceType{ElType: elType}
	case *ast.ReferenceOf:
		e := expr.(*ast.ReferenceOf)
		analyzeExpression(e.Target)
		if e.Target.Type() == nil {
			analysisError(e.AndToken, "Cannot take reference of void expression.")
		}
		e.Typ = &ast.PointerType{ElType: e.Target.Type()}
	case *ast.Dereference:
		e := expr.(*ast.Dereference)
		analyzeExpression(e.Expression)

		ptrType, isPtr := e.Expression.Type().(*ast.PointerType)
		boxType, isBox := e.Expression.Type().(*ast.BoxType)

		if isPtr {
			e.Typ = ptrType.ElType
		} else if isBox {
			e.Typ = boxType.ElType
		} else {
			analysisError(
				e.CaretToken,
				fmt.Sprintf(
					"Dereference of non-pointer type: '%s'",
					e.Expression.Type().String(),
				),
			)
		}

	case *ast.Literal:
		e := expr.(*ast.Literal)
		switch e.Token.Type {
		case lexer.STRING:
			e.Typ = &ast.Primitive{Name: "str"}
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

var (
	// Global used to track what expressions are valid return values.
	functionScope *ast.FunctionDeclaration

	// Tracks whether the current function has a valid return statement.
	hasValidReturn bool

	// Track whether we are within a function or not
	isTopLevel = true
)

func analyzeStatement(statement ast.Statement) {
	switch statement.(type) {
	case *ast.FunctionDeclaration:
		s := statement.(*ast.FunctionDeclaration)

		if !isTopLevel {
			analysisError(s.Identifier, "Nested functions are not yet supported.")
		}

		oldFunctionScope := functionScope
		functionScope = s

		onlyTypeParams := []ast.Type{}
		for _, p := range s.Parameters {
			onlyTypeParams = append(onlyTypeParams, p.Type)
			if st, ok := p.Type.(*ast.StructType); ok {
				resolveStructType(st)
			}
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
		isTopLevel = false

		for _, param := range s.Parameters {
			namespace.shadow(param.Identifier.Lexeme, param.Type)
		}

		for _, statement := range s.Block.Statements {
			analyzeStatement(statement)
		}

		namespace = oldScope
		functionScope = oldFunctionScope
		isTopLevel = true

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
			if st, ok := m.Type.(*ast.StructType); ok {
				if st.Name == s.Identifier.Lexeme {
					analysisError(m.Identifier, "Cannot nest struct type as size would be unknown.")
				}

				resolveStructType(st)
			} else if _, ok := m.Type.(*ast.PointerType); ok {
				analysisError(m.Identifier, "Cannot use pointer as struct field.\n  help: try using a box instead.")
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

		if isTopLevel {
			analysisError(s.Identifier, "Global variables are not supported. Variables must be enclosed in a function's scope.")
		}

		if s.Value != nil {
			analyzeExpression(s.Value)
		}

		if s.Type == nil && s.Value == nil {
			// e.g. var foo
			analysisError(s.Identifier, "Variable declaration must have either type or initial value.")
		} else if s.Type == nil && s.Value != nil {
			// e.g. var foo = []
			if sliceLit, ok := s.Value.(*ast.SliceLiteral); ok && len(sliceLit.Expressions) == 0 {
				analysisError(s.Identifier, "Cannot infer type of 0 element slice literal.")
			}

			// // e.g. var foo = nil
			// if l, ok := s.Value.(*ast.Literal); ok && l.LiteralType == lexer.NIL {
			// 	analysisError(s.Identifier, "Cannot infer type of variable initialized with `nil`.")
			// }

			// e.g. var foo = 42
			s.Type = s.Value.Type()
		} else if s.Type != nil && s.Value != nil {
			// e.g. var foo string = 42
			err := isAssignable(s.Value.Type(), s.Type, isLvalue(s.Value))

			if err != nil {
				analysisError(s.Identifier, "Initial expression is not valid for provided type. "+err.Error())
			}
		}
		// We do nothing in the final case:
		// e.g. var foo int

		err := namespace.declare(s.Identifier.Lexeme, s.Type)
		if err != nil {
			analysisError(s.Identifier, err.Error())
		}
	case *ast.IfStatement:
		s := statement.(*ast.IfStatement)

		if isTopLevel {
			analysisError(s.IfToken, "If statement cannot be top level.")
		}

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

		if isTopLevel {
			analysisError(s.WhileToken, "While statement cannot be top level.")
		}

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

		if isTopLevel {
			analysisError(s.PrintToken, "Print statement cannot be top level.")
		}

		for _, expr := range s.Expressions {
			analyzeExpression(expr)
		}
	case *ast.ReturnStatement:
		s := statement.(*ast.ReturnStatement)

		if isTopLevel {
			analysisError(s.ReturnToken, "Return statement outside function.")
		}

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
			err := isAssignable(s.Expression.Type(), functionScope.ReturnType, isLvalue(s.Expression))

			if err != nil {
				analysisError(
					s.ReturnToken,
					"Mismatched return type. "+err.Error(),
				)
			}
		}

		hasValidReturn = true
	case *ast.ExpressionStatement:
		s := statement.(*ast.ExpressionStatement)

		if isTopLevel {
			analysisError(s.Expression.ErrorToken(), "Expression statement cannot be top level.")
		}

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
