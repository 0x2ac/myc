package analyzer

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"path/filepath"

	"github.com/kartiknair/myc/pkg/ast"
	"github.com/kartiknair/myc/pkg/lexer"
	"github.com/kartiknair/myc/pkg/parser"
	"github.com/kartiknair/myc/pkg/token"
)

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

func (s *SymbolTable) Get(name string) (ast.Type, error) {
	if value, ok := s.values[name]; ok {
		return value, nil
	} else if s.enclosing == nil {
		return nil, errors.New("Value with that name is not declared.")
	}

	return s.enclosing.Get(name)
}

func (s *SymbolTable) Declare(name string, initialType ast.Type) error {
	if _, err := s.Get(name); err == nil {
		return errors.New("Redeclaration of variable.")
	}

	s.values[name] = initialType
	return nil
}

func (s *SymbolTable) Shadow(name string, typ ast.Type) {
	s.values[name] = typ
}

type Analyzer struct {
	Module *ast.Module

	namespace *SymbolTable
	typespace *SymbolTable
}

// Modifies the `statements` argument to add and check type information.
// Panics with a useful error message if program has semantic errors.
//
// Note that we take the statements slice by value since we will not be
// modifying the slice header and only need to modify the elements (and
// therefore the underying buffer).
func Analyze(m *ast.Module) {
	a := Analyzer{
		Module:    m,
		namespace: NewSymbolTable(),
		typespace: NewSymbolTable(),
	}

	for _, statement := range m.Statements {
		a.analyzeStatement(statement)

		if f, ok := statement.(*ast.FunctionDeclaration); ok {
			if f.Identifier.Lexeme == "main" {
				// Special checking for top-level main function
				if f.ReturnType != nil {
					a.analysisError(f.Identifier, "Top level main function must not return anything.")
				} else if len(f.Parameters) != 0 {
					a.analysisError(f.Identifier, "Top level main function cannot take any parameters.")
				}
			}
		}
	}
}

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

	if targetType == nil {
		panic("Invalid call to `isAssignable`, target-type must not be `nil`.")
	} else if valueType == nil && targetType != nil {
		return errors.New(fmt.Sprintf("Void expression cannot be used for type: '%s'.", targetType.String()))
	} else if !valueType.Equals(targetType) {
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

func (a *Analyzer) analysisError(token token.Token, message string) {
	log.Fatalf(
		"%s\nanalysis-error: %d:%d: %s",
		a.Module.TokenSourceContext(&token),
		token.Pos.Line, token.Pos.Column, message,
	)
}

// Makes sure that a programmer-provided type annotation is correct
func (a *Analyzer) analyzeType(t ast.Type) error {
	if sumType, ok := t.(*ast.SumType); ok {
		for _, o := range sumType.Options {
			e := a.analyzeType(o)
			if e != nil {
				return e
			}
		}
	} else if boxType, ok := t.(*ast.BoxType); ok {
		return a.analyzeType(boxType.ElType)
	} else if ptrType, ok := t.(*ast.PointerType); ok {
		return a.analyzeType(ptrType.ElType)
	} else if structType, ok := t.(*ast.StructType); ok {
		if structType.SourceModule != nil {
			resolvedModuleType, err := a.namespace.Get(structType.SourceModule.Name)

			if err != nil {
				return err
			} else if _, ok := resolvedModuleType.(*ast.Module); !ok {
				return errors.New(fmt.Sprintf("%s is not a module.", resolvedModuleType.String()))
			}

			structType.SourceModule = resolvedModuleType.(*ast.Module)

			resolvedType, ok := structType.SourceModule.Exports[structType.Name]
			if !ok {
				return errors.New(fmt.Sprintf(
					"Type: '%s' is not exported from module: '%s'.",
					structType.Name, structType.SourceModule.Name,
				))
			}

			structType.Members = resolvedType.(*ast.StructType).Members
		} else {
			resolved, err := a.typespace.Get(structType.Name)
			if err != nil {
				return err
			}

			*structType = *resolved.(*ast.StructType)
		}
	}

	return nil
}

func (a *Analyzer) analyzeExpression(expr ast.Expression) {
	switch expr.(type) {
	case *ast.UnaryExpression:
		e := expr.(*ast.UnaryExpression)
		a.analyzeExpression(e.Value)
		if _, ok := e.Value.Type().(*ast.Primitive); !ok {
			// The value is not a primitive.
			a.analysisError(e.Operator, "Unary operator can only be on primitive types.")
		}

		prim := e.Value.Type().(*ast.Primitive)
		if prim.Name != "int" && prim.Name != "float" {
			a.analysisError(e.Operator, "Unary operator can only be on `int` or `float` types.")
		}

		e.Typ = prim
	case *ast.BinaryExpression:
		e := expr.(*ast.BinaryExpression)
		a.analyzeExpression(e.Left)
		a.analyzeExpression(e.Right)

		if e.Operator.Type == token.EQUAL {
			_, isVariableExpr := e.Left.(*ast.VariableExpression)
			_, isGetExpr := e.Left.(*ast.GetExpression)
			_, isDeref := e.Left.(*ast.Dereference)

			targetIsLvalue := isVariableExpr || isGetExpr || isDeref

			if !targetIsLvalue {
				a.analysisError(e.Operator, "Target for assignment is not lvalue.")
			}

			err := isAssignable(e.Right.Type(), e.Left.Type(), isLvalue(e.Right))
			if err != nil {
				a.analysisError(e.Operator, err.Error())
			}
		} else if e.Operator.Type != token.EQUAL {
			if !e.Left.Type().Equals(e.Right.Type()) {
				a.analysisError(e.Operator, "Binary expressions must have the same type on both sides.")
			}

			prim, ok := e.Left.Type().(*ast.Primitive)
			if !ok {
				a.analysisError(e.Operator, fmt.Sprintf(
					"Operator: '%s' can only be used on primitive types.",
					e.Operator.Lexeme,
				))
			}

			if e.Operator.Type == token.PLUS {
				if !prim.IsNumeric() && prim.Name != "str" {
					a.analysisError(e.Operator, fmt.Sprintf(
						"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`) and `str`s.",
						e.Operator.Lexeme,
					))
				}

				e.Typ = e.Left.Type()
			} else if e.Operator.Type == token.MINUS ||
				e.Operator.Type == token.STAR ||
				e.Operator.Type == token.SLASH {
				// Arithmetic operators are only for numeric expressions
				if !prim.IsNumeric() {
					a.analysisError(e.Operator, fmt.Sprintf(
						"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`).",
						e.Operator.Lexeme,
					))
				}

				e.Typ = e.Left.Type()
			} else if e.Operator.Type == token.LESSER ||
				e.Operator.Type == token.GREATER ||
				e.Operator.Type == token.LESSER_EQUAL ||
				e.Operator.Type == token.GREATER_EQUAL {
				// Comparison operators are also for numeric expressions
				if !prim.IsNumeric() {
					a.analysisError(e.Operator, fmt.Sprintf(
						"Operator: '%s' can only be used on numeric primitives (e.g. `int`, `float`).",
						e.Operator.Lexeme,
					))
				}

				e.Typ = &ast.Primitive{Name: "bool"}
			} else if e.Operator.Type == token.EQUAL_EQUAL ||
				e.Operator.Type == token.BANG_EQUAL {
				e.Typ = &ast.Primitive{Name: "bool"}
			} else if e.Operator.Type == token.AND_AND || e.Operator.Type == token.OR_OR {
				// Logical operators only work on `bool` expressions
				if prim.Name != "bool" {
					a.analysisError(e.Operator, fmt.Sprintf(
						"Operator: '%s' can only be used on values of type `bool`.",
						e.Operator.Lexeme,
					))
				}

				e.Typ = &ast.Primitive{Name: "bool"}
			}
		}
	case *ast.CallExpression:
		e := expr.(*ast.CallExpression)
		a.analyzeExpression(e.Callee)

		if _, ok := e.Callee.Type().(*ast.FunctionType); !ok {
			// The callee is not a function.
			a.analysisError(
				e.LeftParenToken,
				fmt.Sprintf(
					"Attempting to call value of non-functional type: `%s`",
					e.Callee.Type().String(),
				),
			)
		}

		signature := e.Callee.Type().(*ast.FunctionType)

		if len(e.Arguments) != len(signature.Parameters) {
			a.analysisError(
				e.LeftParenToken,
				fmt.Sprintf(
					"Function being called requires %d parameters. Received %d arguments instead.",
					len(signature.Parameters),
					len(e.Arguments),
				),
			)
		}

		for i, param := range signature.Parameters {
			a.analyzeExpression(e.Arguments[i])

			allowLvalueAssignment := true

			err := isAssignable(e.Arguments[i].Type(), param, isLvalue(e.Arguments[i]), allowLvalueAssignment)

			if err != nil {
				a.analysisError(
					e.LeftParenToken,
					"Mismatched type for positional argument. "+err.Error(),
				)
			}
		}

		e.Typ = signature.ReturnType
	case *ast.VariableExpression:
		e := expr.(*ast.VariableExpression)
		valueType, err := a.namespace.Get(e.Identifier.Lexeme)
		if err != nil {
			a.analysisError(e.Identifier, err.Error())
		}
		e.Typ = valueType
	case *ast.GetExpression:
		e := expr.(*ast.GetExpression)

		a.analyzeExpression(e.Expression)

		structType, isStruct := e.Expression.Type().(*ast.StructType)
		moduleType, isModule := e.Expression.Type().(*ast.Module)

		// e.g. 32.value, "hello".foo, etc.
		if !isStruct && !isModule {
			a.analysisError(
				e.Identifier,
				fmt.Sprintf(
					"Expected structure/module type, instead got type: '%s'.",
					e.Expression.Type().String(),
				),
			)
		}

		if isStruct {
			foundMember, ok := structType.GetMember(e.Identifier.Lexeme)
			if !ok {
				a.analysisError(
					e.Identifier,
					fmt.Sprintf(
						"Type: '%s' does not have field: '%s'.",
						structType.Name,
						e.Identifier.Lexeme,
					),
				)
			}

			e.Typ = foundMember.Type
		} else {
			foundDeclType, ok := moduleType.Exports[e.Identifier.Lexeme]

			if !ok {
				a.analysisError(e.Identifier, fmt.Sprintf(
					"Module: '%s' does not export member: '%s'.",
					moduleType.Path, e.Identifier.Lexeme,
				))
			}

			e.Typ = foundDeclType
		}
	case *ast.IndexExpression:
		e := expr.(*ast.IndexExpression)
		a.analyzeExpression(e.Expression)
		a.analyzeExpression(e.Index)

		st, exprIsSlice := e.Expression.Type().(*ast.SliceType)
		prim, indexIsPrimitive := e.Index.Type().(*ast.Primitive)

		if !exprIsSlice {
			a.analysisError(
				e.LeftBracketToken,
				fmt.Sprintf(
					"Index operation can only be done on slice type values. Received value of type: '%s' instead",
					e.Expression.Type().String(),
				),
			)
		} else if !indexIsPrimitive || prim.Name != "int" {
			a.analysisError(
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

		a.analyzeExpression(e.Expression)
		a.analyzeType(e.TargetType)

		if sumType, ok := e.Expression.Type().(*ast.SumType); !ok {
			a.analysisError(e.AsToken, "Can only type cast sum-type expressions.")
		} else {
			foundOption := false
			for _, o := range sumType.Options {
				if o.Equals(e.TargetType) {
					foundOption = true
					break
				}
			}

			if !foundOption {
				a.analysisError(e.AsToken, fmt.Sprintf(
					"Cannot cast expression of sum-type: '%s' to type: '%s'.",
					sumType.String(), e.TargetType.String(),
				))
			}
		}

		e.Typ = e.TargetType
	case *ast.IsExpression:
		e := expr.(*ast.IsExpression)

		a.analyzeExpression(e.Expression)
		a.analyzeType(e.ComparedType)

		if sumType, ok := e.Expression.Type().(*ast.SumType); !ok {
			a.analysisError(e.IsToken, "Can only type check sum-type expressions.")
		} else {
			foundOption := false
			for _, o := range sumType.Options {
				if o.Equals(e.ComparedType) {
					foundOption = true
					break
				}
			}

			if !foundOption {
				a.analysisError(e.IsToken, fmt.Sprintf(
					"Unnecessary type-check, sum-type: '%s' can never be type: '%s'.",
					sumType.String(), e.ComparedType.String(),
				))
			}
		}

		e.Typ = &ast.Primitive{Name: "bool"}
	case *ast.CompositeLiteral:
		e := expr.(*ast.CompositeLiteral)

		err := a.analyzeType(e.Typ)
		if err != nil {
			a.analysisError(e.LeftBraceToken, "Invalid type in composite literal. "+err.Error())
		}

		r := e.Typ.(*ast.StructType)

		if e.NamedInitializers == nil && e.UnnamedInitializers == nil {
			a.analysisError(
				e.LeftBraceToken,
				"Missing intializers for members in composite literal.",
			)
		} else if e.NamedInitializers != nil {
			if len(*e.NamedInitializers) != len(r.Members) {
				a.analysisError(
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
				a.analyzeExpression(initializer.Value)

				resolvedMember, ok := r.GetMember(initializer.Identifier.Lexeme)

				// Check if the resolved type has a field with that name
				if !ok {
					a.analysisError(
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
					a.analysisError(
						initializer.Identifier,
						"Mismatched type for struct field. "+err.Error(),
					)
				}
			}
		} else if e.UnnamedInitializers != nil {
			if len(*e.UnnamedInitializers) != len(r.Members) {
				a.analysisError(
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
				a.analyzeExpression(initializer)

				err := isAssignable(initializer.Type(), r.Members[i].Type, isLvalue(initializer))

				if err != nil {
					a.analysisError(
						e.LeftBraceToken,
						"Positional initializer has incorrect type. "+err.Error(),
					)
				}
			}
		}
	case *ast.SliceLiteral:
		e := expr.(*ast.SliceLiteral)

		var elType ast.Type
		for _, initializer := range e.Expressions {
			a.analyzeExpression(initializer)
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
		a.analyzeExpression(e.Target)
		if e.Target.Type() == nil {
			a.analysisError(e.AndToken, "Cannot take reference of void expression.")
		}
		e.Typ = &ast.PointerType{ElType: e.Target.Type()}
	case *ast.Dereference:
		e := expr.(*ast.Dereference)
		a.analyzeExpression(e.Expression)

		ptrType, isPtr := e.Expression.Type().(*ast.PointerType)
		boxType, isBox := e.Expression.Type().(*ast.BoxType)

		if isPtr {
			e.Typ = ptrType.ElType
		} else if isBox {
			e.Typ = boxType.ElType
		} else {
			a.analysisError(
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
		case token.STRING:
			e.Typ = &ast.Primitive{Name: "str"}
		case token.INT:
			e.Typ = &ast.Primitive{Name: "int"}
		case token.FLOAT:
			e.Typ = &ast.Primitive{Name: "float"}
		case token.TRUE:
			fallthrough
		case token.FALSE:
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

func (a *Analyzer) typeIsPrivate(t ast.Type) bool {
	if structType, ok := t.(*ast.StructType); ok {
		// TODO
		fmt.Println(structType)
	} else if sliceType, ok := t.(*ast.SliceType); ok {
		return a.typeIsPrivate(sliceType.ElType)
	} else if ptrType, ok := t.(*ast.PointerType); ok {
		return a.typeIsPrivate(ptrType.ElType)
	} else if boxType, ok := t.(*ast.BoxType); ok {
		return a.typeIsPrivate(boxType.ElType)
	}

	return true
}

func (a *Analyzer) analyzeStatement(statement ast.Statement) {
	switch statement.(type) {
	case *ast.FunctionDeclaration:
		s := statement.(*ast.FunctionDeclaration)

		if !isTopLevel {
			a.analysisError(s.Identifier, "Nested functions are not yet supported.")
		}

		oldFunctionScope := functionScope
		functionScope = s

		onlyTypeParams := []ast.Type{}
		for _, p := range s.Parameters {
			onlyTypeParams = append(onlyTypeParams, p.Type)
			err := a.analyzeType(p.Type)
			if err != nil {
				a.analysisError(p.Identifier, "Invalid parameter type. "+err.Error())
			}
		}

		err := a.analyzeType(s.ReturnType)
		if err != nil {
			a.analysisError(s.Identifier, "Invalid return type. "+err.Error())
		}
		if _, ok := s.ReturnType.(*ast.PointerType); ok {
			a.analysisError(s.Identifier, "Cannot return pointer type.\n  help: consider returning a box type instead")
		}

		signature := ast.FunctionType{
			Name:       s.Identifier.Lexeme,
			Parameters: onlyTypeParams,
			ReturnType: s.ReturnType,
		}

		err = a.namespace.Declare(s.Identifier.Lexeme, &signature)
		if err != nil {
			a.analysisError(s.Identifier, err.Error())
		}

		oldScope := a.namespace // copy the old scope
		a.namespace = NewSymbolTableFromEnclosing(a.namespace)
		isTopLevel = false

		for _, param := range s.Parameters {
			a.namespace.Shadow(param.Identifier.Lexeme, param.Type)
		}

		for _, statement := range s.Block.Statements {
			a.analyzeStatement(statement)
		}

		a.namespace = oldScope
		functionScope = oldFunctionScope
		isTopLevel = true

		if !hasValidReturn && s.ReturnType != nil {
			a.analysisError(
				s.Identifier,
				fmt.Sprintf(
					"Function does not have a return statement. Expected return of type: `%s`.",
					s.ReturnType,
				),
			)
		}

		if s.Exported {
			a.Module.Exports[s.Identifier.Lexeme] = &signature
		}
	case *ast.StructDeclaration:
		s := statement.(*ast.StructDeclaration)

		for _, m := range s.Members {
			err := a.analyzeType(m.Type)
			if err != nil {
				a.analysisError(m.Identifier, "Invalid type for struct member. "+err.Error())
			}

			if _, ok := m.Type.(*ast.PointerType); ok {
				a.analysisError(m.Identifier, "Cannot use pointer as struct field.\n  help: try using a box instead.")
			}
		}

		st := ast.StructType{
			Name:         s.Identifier.Lexeme,
			Members:      s.Members,
			SourceModule: a.Module,
		}

		err := a.typespace.Declare(s.Identifier.Lexeme, &st)
		if err != nil {
			a.analysisError(s.Identifier, fmt.Sprintf("Type name '%s' is already in use.", s.Identifier.Lexeme))
		}

		if s.Exported {
			a.Module.Exports[s.Identifier.Lexeme] = &st
		}
	case *ast.VariableDeclaration:
		s := statement.(*ast.VariableDeclaration)

		if isTopLevel {
			a.analysisError(s.Identifier, "Global variables are not supported. Variables must be enclosed in a function's scope.")
		}

		if s.Value != nil {
			a.analyzeExpression(s.Value)
		}

		if s.Type == nil && s.Value == nil {
			// e.g. var foo
			a.analysisError(s.Identifier, "Variable declaration must have either type or initial value.")
		} else if s.Type == nil && s.Value != nil {
			// e.g. var foo = []
			if sliceLit, ok := s.Value.(*ast.SliceLiteral); ok && len(sliceLit.Expressions) == 0 {
				a.analysisError(s.Identifier, "Cannot infer type of 0 element slice literal.")
			}

			if s.Value.Type() == nil {
				a.analysisError(s.Value.ErrorToken(), "Cannot create variable from void expression.")
			}

			// e.g. var foo = 42
			s.Type = s.Value.Type()
		} else if s.Type != nil && s.Value != nil {
			// e.g. var foo string = 42
			err := a.analyzeType(s.Type)
			if err != nil {
				a.analysisError(s.Identifier, "Invalid variable type. "+err.Error())
			}

			err = isAssignable(s.Value.Type(), s.Type, isLvalue(s.Value))
			if err != nil {
				a.analysisError(s.Identifier, "Initial expression is not valid for provided type. "+err.Error())
			}
		}
		// We do nothing in the final case:
		// e.g. var foo int

		err := a.namespace.Declare(s.Identifier.Lexeme, s.Type)
		if err != nil {
			a.analysisError(s.Identifier, err.Error())
		}
	case *ast.IfStatement:
		s := statement.(*ast.IfStatement)

		if isTopLevel {
			a.analysisError(s.IfToken, "If statement cannot be top level.")
		}

		a.analyzeExpression(s.Condition)
		if p, ok := s.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
			a.analysisError(
				s.IfToken,
				fmt.Sprintf(
					"If statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
					s.Condition.Type().String(),
				),
			)
		}
		a.analyzeStatement(&s.IfBlock)

		if len(s.ElseIfStatements) != 0 {
			for _, e := range s.ElseIfStatements {
				a.analyzeExpression(e.Condition)
				if p, ok := e.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
					a.analysisError(
						e.IfToken,
						fmt.Sprintf(
							"Else if statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
							e.Condition.Type().String(),
						),
					)
				}
				a.analyzeStatement(&e.Block)
			}
		}

		if s.ElseBlock != nil {
			a.analyzeStatement(s.ElseBlock)
		}
	case *ast.WhileStatement:
		s := statement.(*ast.WhileStatement)

		if isTopLevel {
			a.analysisError(s.WhileToken, "While statement cannot be top level.")
		}

		a.analyzeExpression(s.Condition)
		if p, ok := s.Condition.Type().(*ast.Primitive); !ok || p.Name != "bool" {
			a.analysisError(
				s.WhileToken,
				fmt.Sprintf(
					"While statement's condition must be of type: `bool`. Got expression of type: '%s' instead.",
					s.Condition.Type().String(),
				),
			)
		}
		a.analyzeStatement(&s.Block)
	case *ast.PrintStatement:
		s := statement.(*ast.PrintStatement)

		if isTopLevel {
			a.analysisError(s.PrintToken, "Print statement cannot be top level.")
		}

		for _, expr := range s.Expressions {
			a.analyzeExpression(expr)
			if expr.Type() == nil {
				a.analysisError(expr.ErrorToken(), "Cannot print void expression")
			}
		}
	case *ast.ReturnStatement:
		s := statement.(*ast.ReturnStatement)

		if isTopLevel {
			a.analysisError(s.ReturnToken, "Return statement outside function.")
		}

		if s.Expression != nil {
			a.analyzeExpression(s.Expression)
		}

		if functionScope == nil {
			a.analysisError(s.ReturnToken, "Return statement not inside a function.")
		} else if s.Expression == nil && functionScope.ReturnType != nil {
			a.analysisError(
				s.ReturnToken,
				fmt.Sprintf(
					"Missing expression in return statement. Function has return type: `%s`.",
					functionScope.ReturnType.String(),
				),
			)
		} else if s.Expression != nil && functionScope.ReturnType == nil {
			a.analysisError(s.ReturnToken, "Returning value in void function.")
		} else if s.Expression != nil {
			err := isAssignable(s.Expression.Type(), functionScope.ReturnType, isLvalue(s.Expression))

			if err != nil {
				a.analysisError(
					s.ReturnToken,
					"Mismatched return type. "+err.Error(),
				)
			}
		}

		hasValidReturn = true
	case *ast.ExpressionStatement:
		s := statement.(*ast.ExpressionStatement)

		if isTopLevel {
			a.analysisError(s.Expression.ErrorToken(), "Expression statement cannot be top level.")
		}

		a.analyzeExpression(s.Expression)
	case *ast.ImportStatement:
		s := statement.(*ast.ImportStatement)

		if !isTopLevel {
			a.analysisError(s.PathToken, "Import statement must be top level.")
		}

		if s.Identifier == nil {
			filename := filepath.Base(s.PathToken.Lexeme)
			filename = filename[:len(filename)-len(filepath.Ext(filename))]
			s.Identifier = &token.Token{Lexeme: filename}
		}

		module, err := resolveModuleFromPath(s.Identifier.Lexeme, a.Module, s.PathToken.Lexeme)
		if err != nil {
			a.analysisError(s.PathToken, "Failed while importing module. "+err.Error())
		}

		a.namespace.Declare(s.Identifier.Lexeme, module)
		a.Module.Imports[s.Identifier.Lexeme] = module
	case *ast.BlockStatement:
		oldScope := a.namespace // copy the old scope
		a.namespace = NewSymbolTableFromEnclosing(a.namespace)

		s := statement.(*ast.BlockStatement)
		for _, stmt := range s.Statements {
			a.analyzeStatement(stmt)
		}

		a.namespace = oldScope
	}
}

func resolveModuleFromPath(alias string, currentModule *ast.Module, path string) (*ast.Module, error) {
	resolvedPath := filepath.Join(filepath.Dir(currentModule.Path), path)
	contents, err := ioutil.ReadFile(resolvedPath)
	if err != nil {
		return nil, err
	}

	m := ast.Module{
		Name:    alias,
		Path:    resolvedPath,
		Source:  string(contents),
		Imports: make(map[string]*ast.Module),
		Exports: make(map[string]ast.Type),
	}

	lexer.Lex(&m)
	parser.Parse(&m)
	Analyze(&m)

	return &m, nil
}
