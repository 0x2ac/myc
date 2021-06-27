package parser

import (
	"log"

	"github.com/kartiknair/myc/ast"
	"github.com/kartiknair/myc/lexer"
)

var (
	current int
	tokens  []lexer.Token
)

func parseError(token lexer.Token, message string) {
	log.Fatalf(
		"[Parser Error: %d:%d] %s",
		token.Pos.Line, token.Pos.Column, message,
	)
}

func peek(distance int) lexer.Token {
	return tokens[current+distance]
}

func expect(typ lexer.TokenType, message string) lexer.Token {
	if peek(0).Type != typ {
		parseError(peek(0), message)
	}

	current++
	return peek(-1)
}

func parseStatement() ast.Statement {
	t := peek(0)

	if t.Type == lexer.FUN {
		// FunctionDeclaration
		current++
		name := expect(lexer.IDENTIFIER, "Expect function name.")
		expect(lexer.LEFT_PAREN, "Expect `(` after function name.")

		parameters := []ast.Parameter{}
		if peek(0).Type != lexer.RIGHT_PAREN {
			for {
				paramName := expect(lexer.IDENTIFIER, "Expect name for function parameter.")
				paramType := parseType()
				parameters = append(parameters, ast.Parameter{
					Identifier: paramName,
					Type:       paramType,
				})

				if peek(0).Type != lexer.COMMA {
					break
				} else {
					current++ // skip the comma
				}
			}
		}
		expect(lexer.RIGHT_PAREN, "Missing closing `)` after parameter list.")

		var returnType ast.Type

		if peek(0).Type != lexer.LEFT_BRACE {
			returnType = parseType()
		}

		expect(lexer.LEFT_BRACE, "Expect block after function signature.")
		current-- // since expect consumes the `{`
		block := parseBlock()

		return &ast.FunctionDeclaration{
			Identifier: name,
			Parameters: parameters,
			ReturnType: returnType,
			Block:      *block,
		}
	} else if t.Type == lexer.VAR {
		// VariableDeclaration
		current++
		name := expect(lexer.IDENTIFIER, "Expect identifier after `var`.")
		var typ ast.Type
		if peek(0).Type != lexer.EQUAL {
			typ = parseType()
		}

		var expr ast.Expression = nil

		if peek(0).Type == lexer.EQUAL {
			current++
			expr = parseExpression()
		}

		expect(lexer.SEMICOLON, "Expect `;` after variable declaration.")
		return &ast.VariableDeclaration{
			Identifier: name,
			Type:       typ,
			Value:      expr,
		}
	} else if t.Type == lexer.CONST {
		// ConstantDeclaration
		current++
		name := expect(lexer.IDENTIFIER, "Expect identifier after `const`.")
		expr := parseExpression()
		return &ast.ConstantDeclaration{
			Identifier: name,
			Value:      expr,
		}
	} else if t.Type == lexer.RETURN {
		// ReturnStatement
		current++
		var expr ast.Expression
		if peek(0).Type != lexer.SEMICOLON {
			expr = parseExpression()
		}
		expect(lexer.SEMICOLON, "Expect semicolon after return statement.")
		return &ast.ReturnStatement{
			Expression:  expr,
			ReturnToken: t,
		}
	} else if t.Type == lexer.PRINT {
		// PrintStatement
		current++
		exprs := []ast.Expression{}
		first := parseExpression()
		exprs = append(exprs, first)
		for peek(0).Type == lexer.COMMA {
			current++
			exprs = append(exprs, parseExpression())
		}
		expect(lexer.SEMICOLON, "Expect semicolon after print statement.")
		return &ast.PrintStatement{
			Expressions: exprs,
		}
	} else if t.Type == lexer.LEFT_BRACE {
		// BlockStatement
		return parseBlock()
	}

	expr := parseExpression()
	expect(lexer.SEMICOLON, "Expect semicolon after top-level expression.")
	// ExpressionStatement
	return &ast.ExpressionStatement{
		Expression: expr,
	}
}

func parseBlock() *ast.BlockStatement {
	current++ // skip the `{`
	statements := []ast.Statement{}
	for peek(0).Type != lexer.RIGHT_BRACE {
		if peek(0).Type == lexer.EOF {
			parseError(peek(0), "Unclosed block.")
		}
		statements = append(statements, parseStatement())
	}
	current++ // skip the `}`
	if peek(0).Type == lexer.SEMICOLON {
		current++ // optional trailing semicolon is allowed
	}
	return &ast.BlockStatement{
		Statements: statements,
	}
}

func parseExpression() ast.Expression {
	return parsePrecedenceExpression(parsePrimary(), 0)
}

type associativity int

const (
	associativeLeft associativity = iota
	associativeRight
)

type opInfo struct {
	precedence    int
	associativity associativity
}

var operatorPrecedenceMap = map[lexer.TokenType]opInfo{
	lexer.EQUAL:   {precedence: 1, associativity: associativeRight},
	lexer.PLUS:    {precedence: 2, associativity: associativeLeft},
	lexer.MINUS:   {precedence: 2, associativity: associativeLeft},
	lexer.PERCENT: {precedence: 2, associativity: associativeLeft},
	lexer.STAR:    {precedence: 3, associativity: associativeLeft},
	lexer.SLASH:   {precedence: 3, associativity: associativeLeft},
}

func parsePrecedenceExpression(lhs ast.Expression, minPrecedence int) ast.Expression {
	lookahead := peek(0)
	for lookahead.Type.IsBinaryOperator() &&
		operatorPrecedenceMap[lookahead.Type].precedence >= minPrecedence {

		op := lookahead
		current++
		rhs := parsePrimary()
		lookahead = peek(0)

		// TODO: Clean this up. This is pretty difficult to read.
		for lookahead.Type.IsBinaryOperator() &&
			(operatorPrecedenceMap[lookahead.Type].associativity ==
				associativeLeft &&
				operatorPrecedenceMap[lookahead.Type].precedence >
					operatorPrecedenceMap[op.Type].precedence) ||
			(operatorPrecedenceMap[lookahead.Type].associativity ==
				associativeRight &&
				operatorPrecedenceMap[lookahead.Type].precedence ==
					operatorPrecedenceMap[op.Type].precedence) {

			rhs = parsePrecedenceExpression(rhs, minPrecedence+1)
			lookahead = peek(0)
		}

		lhs = &ast.BinaryExpression{Left: lhs, Operator: op, Right: rhs}
	}

	return lhs
}

func parsePrimary() ast.Expression {
	var expr ast.Expression

	if peek(0).Type == lexer.INT || peek(0).Type == lexer.FLOAT {
		current++
		expr = &ast.Literal{
			LiteralType:  peek(-1).Type,
			LiteralValue: peek(-1).Lexeme,
		}
	} else if peek(0).Type == lexer.IDENTIFIER {
		current++
		expr = &ast.VariableExpression{
			Identifier: peek(-1),
		}
	} else if peek(0).Type == lexer.MINUS {
		current++
		expr = &ast.UnaryExpression{
			Operator: peek(-1),
			Value:    parsePrimary(),
		}
	}

	// Looping to catch multiple calls
	// e.g. foo()()()
	for peek(0).Type == lexer.LEFT_PAREN {
		// CallExpression
		leftParenToken := peek(0)

		current++
		arguments := []ast.Expression{}

		if peek(0).Type != lexer.RIGHT_PAREN {
			for {
				arguments = append(arguments, parseExpression())

				if peek(0).Type != lexer.COMMA {
					break
				} else {
					current++ // skip the comma
				}
			}
		}

		expect(lexer.RIGHT_PAREN, "Missing closing parenthesis in function call.")
		expr = &ast.CallExpression{
			Callee:         expr,
			Arguments:      arguments,
			LeftParenToken: leftParenToken,
		}
	}

	if expr == nil {
		parseError(peek(0), "Expected expression.")
	}

	return expr
}

func parseType() ast.Type {
	if peek(0).Type == lexer.IDENTIFIER {
		if peek(0).Lexeme != "int" && peek(0).Lexeme != "float" {
			parseError(peek(0), "Only types available are: `int` and `float`.")
		}
		current++
		return &ast.Primitive{
			Name: peek(-1).Lexeme,
		}
	}

	parseError(peek(0), "Only named types are allowed.")
	panic("")
}

func Parse(inputTokens []lexer.Token) []ast.Statement {
	result := []ast.Statement{}
	tokens = inputTokens

	for peek(0).Type != lexer.EOF {
		result = append(result, parseStatement())
	}

	return result
}
