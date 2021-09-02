package cgen

import (
	"fmt"

	"github.com/kartiknair/myc/pkg/ast"
)

func genType(t ast.Type) string {
	switch t.(type) {
	case *ast.Primitive:
		return t.(*ast.Primitive).String()
	case *ast.StructType:
		return t.(*ast.StructType).Name
	case *ast.PointerType:
		return fmt.Sprintf("%s*", genType(t.(*ast.PointerType).ElType))
	case *ast.SliceType:
		st := t.(*ast.SliceType)
		return fmt.Sprintf(
			"struct {%s* buffer; u64 length; u64 capacity;}",
			genType(st.ElType),
		)
	}

	panic("Type node has invalid static type.")
}

func genStatement(stmt ast.Statement) string {
	switch stmt.(type) {
	case *ast.StructDeclaration:
		return genStructDeclaration(stmt.(*ast.StructDeclaration))
	case *ast.FunctionDeclaration:
		return genFunctionDeclaration(stmt.(*ast.FunctionDeclaration))
	case *ast.VariableDeclaration:
		return genVariableDeclaration(stmt.(*ast.VariableDeclaration))
	case *ast.IfStatement:
		return genIfStatement(stmt.(*ast.IfStatement))
	case *ast.WhileStatement:
		return genWhileStatement(stmt.(*ast.WhileStatement))
	case *ast.ReturnStatement:
		return genReturnStatement(stmt.(*ast.ReturnStatement))
	case *ast.ExpressionStatement:
		return genExpressionStatement(stmt.(*ast.ExpressionStatement))
	case *ast.BlockStatement:
		return genBlockStatement(stmt.(*ast.BlockStatement))
	}

	panic("Statement node has invalid static type.")
}

func genStructDeclaration(decl *ast.StructDeclaration) string {
	membersString := ""

	for _, m := range decl.Members {
		membersString += genType(m.Type) + " " + m.Identifier.Lexeme + ";"
	}

	return fmt.Sprintf(
		"typedef struct %s %s;struct %s {%s};",
		decl.Identifier.Lexeme,
		decl.Identifier.Lexeme,
		decl.Identifier.Lexeme,
		membersString,
	)
}

func genFunctionDeclaration(decl *ast.FunctionDeclaration) string {
	gennedParameters := ""

	if len(decl.Parameters) == 0 {
		gennedParameters = "void"
	}

	for i, param := range decl.Parameters {
		gennedParameters += genType(param.Type) + " " + param.Identifier.Lexeme
		if i != len(decl.Parameters)-1 {
			gennedParameters += ", "
		}
	}

	ret := ""
	if decl.ReturnType == nil {
		ret = "void"
	} else {
		ret = genType(decl.ReturnType)
	}

	return fmt.Sprintf(
		"%s %s(%s) %s",
		ret,
		decl.Identifier.Lexeme,
		gennedParameters,
		genBlockStatement(decl.Block),
	)
}

func genVariableDeclaration(decl *ast.VariableDeclaration) string {
	return fmt.Sprintf(
		"%s %s = %s;",
		genType(decl.Type),
		decl.Identifier.Lexeme,
		genExpression(decl.Value),
	)
}

func genIfStatement(s *ast.IfStatement) string {
	elifStmts := ""

	for _, elifStmt := range s.ElseIfStatements {
		elifStmts += fmt.Sprintf(
			"else if (%s) %s",
			genExpression(elifStmt.Condition),
			genBlockStatement(&elifStmt.Block),
		)
	}

	elseStmt := ""

	if s.ElseBlock != nil {
		elseStmt += fmt.Sprintf("else %s", genBlockStatement(s.ElseBlock))
	}

	return fmt.Sprintf(
		"if (%s) %s %s %s",
		genExpression(s.Condition),
		genBlockStatement(&s.IfBlock),
		elifStmts,
		elseStmt,
	)
}

func genWhileStatement(s *ast.WhileStatement) string {
	return fmt.Sprintf(
		"while (%s) %s",
		genExpression(s.Condition),
		genBlockStatement(&s.Block),
	)
}

func getFormatStringForType(t ast.Type) string {
	if primitive, ok := t.(*ast.Primitive); ok {
		switch primitive.Kind {
		case ast.I8:
			fallthrough
		case ast.I16:
			fallthrough
		case ast.I32:
			fallthrough
		case ast.I64:
			return "%d"

		case ast.U8:
			fallthrough
		case ast.U16:
			fallthrough
		case ast.U32:
			fallthrough
		case ast.U64:
			return "%ld"

		case ast.F32:
			fallthrough
		case ast.F64:
			return "%.15g"

		case ast.Str:
			return "%s"
		case ast.Bool:
			return "%d"

		}
	} else if _, ok := t.(*ast.StructType); ok {
		return "%p"
	} else if _, ok := t.(*ast.SliceType); ok {
		return "%p"
	}

	panic("Invalid type passed to `getFormatStringForType`.")
}

func genReturnStatement(returnStmt *ast.ReturnStatement) string {
	return fmt.Sprintf("return %s;", genExpression(returnStmt.Expression))
}

func genExpressionStatement(exprStmt *ast.ExpressionStatement) string {
	return fmt.Sprintf("%s;", genExpression(exprStmt.Expression))
}

func genBlockStatement(blockStmt *ast.BlockStatement) string {
	gennedStatements := ""
	for _, statement := range blockStmt.Statements {
		gennedStatements += genStatement(statement)
	}
	return fmt.Sprintf("{%s}", gennedStatements)
}

func genExpression(expr ast.Expression) string {
	switch expr.(type) {
	case *ast.UnaryExpression:
		return genUnaryExpression(expr.(*ast.UnaryExpression))
	case *ast.BinaryExpression:
		return genBinaryExpression(expr.(*ast.BinaryExpression))
	case *ast.CallExpression:
		return genCallExpression(expr.(*ast.CallExpression))
	case *ast.VariableExpression:
		return genVariableExpression(expr.(*ast.VariableExpression))
	case *ast.GetExpression:
		return genGetExpression(expr.(*ast.GetExpression))
	case *ast.IndexExpression:
		return genIndexExpression(expr.(*ast.IndexExpression))
	case *ast.CompositeLiteral:
		return genCompositeLiteral(expr.(*ast.CompositeLiteral))
	case *ast.SliceLiteral:
		return genSliceLiteral(expr.(*ast.SliceLiteral))
	case *ast.ReferenceOf:
		return genReferenceOf(expr.(*ast.ReferenceOf))
	case *ast.Dereference:
		return genDereference(expr.(*ast.Dereference))
	case *ast.Literal:
		return genLiteral(expr.(*ast.Literal))
	}

	panic("Expression node has invalid static type.")
}

func genUnaryExpression(unaryExpr *ast.UnaryExpression) string {
	return fmt.Sprintf("(%s%s)", unaryExpr.Operator.Lexeme, genExpression(unaryExpr.Value))
}

func genBinaryExpression(binaryExpr *ast.BinaryExpression) string {
	return fmt.Sprintf(
		"(%s %s %s)",
		genExpression(binaryExpr.Left),
		binaryExpr.Operator.Lexeme,
		genExpression(binaryExpr.Right),
	)
}

func genCallExpression(callExpr *ast.CallExpression) string {
	gennedArguments := ""

	for i, arg := range callExpr.Arguments {
		gennedArguments += genExpression(arg)
		if i != len(callExpr.Arguments)-1 {
			gennedArguments += ", "
		}
	}

	return fmt.Sprintf(
		"%s(%s)",
		genExpression(callExpr.Callee),
		gennedArguments,
	)
}

func genVariableExpression(varExpr *ast.VariableExpression) string {
	return varExpr.Identifier.Lexeme
}

func genGetExpression(getExpr *ast.GetExpression) string {
	return fmt.Sprintf("%s.%s", genExpression(getExpr.Expression), getExpr.Identifier.Lexeme)
}

func genIndexExpression(indexExpr *ast.IndexExpression) string {
	return fmt.Sprintf(
		"slice__%s__at(%s)",
		genType(indexExpr.Expression.Type().(*ast.SliceType).ElType),
		genExpression(indexExpr.Index),
	)
}

func genCompositeLiteral(compExpr *ast.CompositeLiteral) string {
	initializerString := ""

	if compExpr.NamedInitializers != nil {
		for i, initializer := range *compExpr.NamedInitializers {
			initializerString += "." + initializer.Identifier.Lexeme + " = " + genExpression(initializer.Value)

			if i != len(*compExpr.NamedInitializers)-1 {
				initializerString += ", "
			}
		}
	} else {
		for i, initializer := range *compExpr.UnnamedInitializers {
			initializerString += genExpression(initializer)

			if i != len(*compExpr.UnnamedInitializers)-1 {
				initializerString += ", "
			}
		}
	}

	return fmt.Sprintf(
		"(%s){%s}",
		compExpr.Typ.(*ast.StructType).Name,
		initializerString,
	)
}

func genSliceLiteral(slit *ast.SliceLiteral) string {
	gennedExprs := ""

	for i, e := range slit.Expressions {
		gennedExprs += genExpression(e)

		if i != len(slit.Expressions)-1 {
			gennedExprs += ", "
		}
	}

	return fmt.Sprintf(
		"slice__%s__from_arr((%s){%s}, %d)",
		genType(slit.Type().(*ast.SliceType).ElType),
		genType(slit.Type().(*ast.SliceType).ElType),
		gennedExprs,
		len(slit.Expressions),
	)
}

func genReferenceOf(ref *ast.ReferenceOf) string {
	isLvalue := false
	if _, ok := ref.Target.(*ast.VariableExpression); ok {
		isLvalue = true
	} else if _, ok := ref.Target.(*ast.GetExpression); ok {
		isLvalue = true
	}

	if !isLvalue {
		// This is kind of hacky, basically using the (type){elements...} syntax
		// to create a temporary array and putting our target as the first element
		// and then taking its address. Really all it should compile to is creating
		// a temporary on the stack and taking its address.
		//
		// We only need this hack because when generating C, unlike LLVM, we don't
		// have access to the current black. So this is a workaround.
		return fmt.Sprintf(
			"&((%s[]){%s}[0])",
			genType(ref.Typ.(*ast.PointerType).ElType),
			genExpression(ref.Target),
		)
	}

	return fmt.Sprintf("&%s", genExpression(ref.Target))
}

func genDereference(deref *ast.Dereference) string {
	return fmt.Sprintf("*%s", genExpression(deref.Expression))
}

func genLiteral(literal *ast.Literal) string {
	return literal.LiteralValue
}

func Gen(m *ast.Module) string {
	result := ""
	for _, statement := range m.Statements {
		result += genStatement(statement)
	}
	return result
}
