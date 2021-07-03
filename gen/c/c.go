package cgen

import (
	"fmt"

	"github.com/kartiknair/myc/ast"
)

func genType(t ast.Type) string {
	switch t.(type) {
	case *ast.Primitive:
		return t.(*ast.Primitive).Name
	case *ast.StructType:
		return t.(*ast.StructType).Name
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

func genStructDeclaration(decl *ast.StructDeclaration) string {
	membersString := ""

	for _, m := range decl.Members {
		membersString += m.Type.String() + " " + m.Identifier.Lexeme + ";"
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
	} else if _, ok := t.(*ast.StructType); ok {
		return "%p"
	}

	panic("Invalid type passed to `getFormatStringForType`.")
}

func genPrintStatement(printStmt *ast.PrintStatement) string {
	formatString := ""
	arguments := ""
	for i, expr := range printStmt.Expressions {
		formatString += getFormatStringForType(expr.Type())

		if _, ok := expr.Type().(*ast.StructType); ok {
			// For now structs only get their reference printed
			arguments += "&" + genExpression(expr)
		} else {
			arguments += genExpression(expr)
		}

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
	case *ast.GetExpression:
		return genGetExpression(expr.(*ast.GetExpression))
	case *ast.CompositeLiteral:
		return genCompositeLiteral(expr.(*ast.CompositeLiteral))
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
