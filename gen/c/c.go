package cgen

import (
	"fmt"

	"github.com/kartiknair/myc/ast"
)

func genType(t ast.Type) string {
	switch t.(type) {
	case *ast.Primitive:
		return genPrimitive(t.(*ast.Primitive))
	}

	panic("Type node has invalid static type.")
}

func genPrimitive(primitive *ast.Primitive) string {
	// Assuming that our primitive type names are predefined in our C code.
	return primitive.Name
}

func genStatement(stmt ast.Statement) string {
	switch stmt.(type) {
	case *ast.FunctionDeclaration:
		return genFunctionDeclaration(stmt.(*ast.FunctionDeclaration))
	case *ast.VariableDeclaration:
		return genVariableDeclaration(stmt.(*ast.VariableDeclaration))
	case *ast.ConstantDeclaration:
		return genConstantDeclaration(stmt.(*ast.ConstantDeclaration))
	case *ast.PrintStatement:
		return genPrintStatement(stmt.(*ast.PrintStatement))
	case *ast.ReturnStatement:
		return genReturnStatement(stmt.(*ast.ReturnStatement))
	case *ast.ExpressionStatement:
		return genExpressionStatement(stmt.(*ast.ExpressionStatement))
	case *ast.BlockStatement:
		return genBlockStatement(stmt.(*ast.BlockStatement))
	}

	panic("Statement node has invalid static type.")
}

func genFunctionDeclaration(decl *ast.FunctionDeclaration) string {
	gennedParameters := ""

	for i, param := range decl.Parameters {
		gennedParameters += param.Type.String() + " " + param.Identifier.Lexeme
		if i != len(decl.Parameters)-1 {
			gennedParameters += ", "
		}
	}

	return fmt.Sprintf(
		"%s %s(%s) %s",
		decl.ReturnType.String(),
		decl.Identifier.Lexeme,
		gennedParameters,
		genBlockStatement(&decl.Block),
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

func genConstantDeclaration(decl *ast.ConstantDeclaration) string {
	return fmt.Sprintf(
		"const %s %s = %s;",
		genType(decl.Value.Type()),
		decl.Identifier.Lexeme,
		genExpression(decl.Value),
	)
}

func getFormatStringForType(t ast.Type) string {
	if primitive, ok := t.(*ast.Primitive); ok {
		switch primitive.Name {
		case "int":
			return "%d"
		case "float":
			return "%f"
		}
	}

	panic("Invalid type passed to `getFormatStringForType`.")
}

func genPrintStatement(printStmt *ast.PrintStatement) string {
	formatString := ""
	arguments := ""
	for i, expr := range printStmt.Expressions {
		formatString += getFormatStringForType(expr.Type())
		arguments += genExpression(expr)
		if i != len(printStmt.Expressions)-1 {
			formatString += " "
			arguments += ", "
		}
	}

	return fmt.Sprintf("printf(\"%s\", %s);", formatString, arguments)
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

func genLiteral(literal *ast.Literal) string {
	return literal.LiteralValue
}

func Gen(statements []ast.Statement) string {
	result := ""
	for _, statement := range statements {
		result += genStatement(statement)
	}
	return result
}
