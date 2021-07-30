package parser

import (
	"fmt"
	"log"
	"path/filepath"

	"github.com/kartiknair/myc/pkg/ast"
	"github.com/kartiknair/myc/pkg/token"
)

type Parser struct {
	current int

	Module *ast.Module
}

var primitives = [...]string{
	"int",
	"float",
	"bool",
	"str",
}

func isIdentifierPrimitive(ident token.Token) bool {
	if ident.Type != token.IDENTIFIER {
		panic("Invalid token passed to `isIdentifierPrimitive`.")
	}

	for _, p := range primitives {
		if ident.Lexeme == p {
			return true
		}
	}

	return false
}

func (p *Parser) parseError(token token.Token, message string) {
	log.Fatalf(
		"%s\nparse-error: %d:%d: %s",
		p.Module.TokenSourceContext(&token),
		token.Pos.Line, token.Pos.Column, message,
	)
}

func (p *Parser) peek(distance int) token.Token {
	return p.Module.Tokens[p.current+distance]
}

func (p *Parser) expect(typ token.TokenType, message string) token.Token {
	if p.peek(0).Type != typ {
		p.parseError(p.peek(0), message)
	}

	p.current++
	return p.peek(-1)
}

func (p *Parser) parseStatement() ast.Statement {
	t := p.peek(0)

	if t.Type == token.FUN {
		// FunctionDeclaration
		p.current++
		name := p.expect(token.IDENTIFIER, "Expect function name.")
		p.expect(token.LEFT_PAREN, "Expect `(` after function name.")

		parameters := []ast.Parameter{}
		if p.peek(0).Type != token.RIGHT_PAREN {
			for {
				paramName := p.expect(token.IDENTIFIER, "Expect name for function parameter.")
				paramType := p.parseType()
				parameters = append(parameters, ast.Parameter{
					Identifier: paramName,
					Type:       paramType,
				})

				if p.peek(0).Type != token.COMMA {
					break
				} else {
					p.current++ // skip the comma
				}
			}
		}
		p.expect(token.RIGHT_PAREN, "Missing closing `)` after parameter list.")

		var returnType ast.Type

		if p.peek(0).Type != token.LEFT_BRACE {
			returnType = p.parseType()
		}

		p.expect(token.LEFT_BRACE, "Expect block after function signature.")
		p.current-- // since expect consumes the `{`
		block := p.parseBlock()

		return &ast.FunctionDeclaration{
			Identifier: name,
			Parameters: parameters,
			ReturnType: returnType,
			Block:      *block,
		}
	} else if t.Type == token.STRUCT {
		// StructDeclaration
		p.current++
		name := p.expect(token.IDENTIFIER, "Expect struct name.")
		p.expect(token.LEFT_BRACE, "Expect `{` after name in struct declaration.")

		if p.peek(0).Type == token.RIGHT_BRACE {
			p.parseError(p.peek(0), "Illegal to declare empty struct.")
		}

		members := []ast.StructMember{}
		for p.peek(0).Type != token.RIGHT_BRACE {
			memberName := p.expect(token.IDENTIFIER, "Expect")
			memberType := p.parseType()
			p.expect(token.SEMICOLON, "Expect semicolon after member declaration in struct.")
			members = append(members, ast.StructMember{
				Identifier: memberName,
				Type:       memberType,
			})
		}

		p.current++ // skip the `}`

		if p.peek(0).Type == token.SEMICOLON {
			p.current++ // optional trailing semicolon
		}

		return &ast.StructDeclaration{
			Identifier: name,
			Members:    members,
		}
	} else if t.Type == token.VAR {
		// VariableDeclaration
		p.current++
		name := p.expect(token.IDENTIFIER, "Expect identifier after `var`.")
		var typ ast.Type
		if p.peek(0).Type != token.EQUAL {
			typ = p.parseType()
		}

		var expr ast.Expression = nil

		if p.peek(0).Type == token.EQUAL {
			p.current++
			expr = p.parseExpression(false)
		}

		p.expect(token.SEMICOLON, "Expect `;` after variable declaration.")
		return &ast.VariableDeclaration{
			Identifier: name,
			Type:       typ,
			Value:      expr,
		}
	} else if t.Type == token.IF {
		// IfStatement
		p.current++
		condition := p.parseExpression(true)
		p.expect(token.LEFT_BRACE, "Expect block after if condition.")
		p.current--
		ifBlock := p.parseBlock()
		elifStmts := []ast.ElseIfStatement{}

		hasElseBlock := false
		var elseBlock *ast.BlockStatement
		for p.peek(0).Type == token.ELSE {
			p.current++
			if p.peek(0).Type == token.IF {
				elifToken := p.peek(0)
				p.current++
				elifCond := p.parseExpression(true)
				elifBlock := p.parseBlock()
				elifStmts = append(elifStmts, ast.ElseIfStatement{
					IfToken:   elifToken,
					Condition: elifCond,
					Block:     *elifBlock,
				})
			} else {
				hasElseBlock = true
				break
			}
		}

		if hasElseBlock {
			p.expect(token.LEFT_BRACE, "Expect block after else.")
			p.current--
			elseBlock = p.parseBlock()
		}

		return &ast.IfStatement{
			IfToken:          t,
			Condition:        condition,
			IfBlock:          *ifBlock,
			ElseIfStatements: elifStmts,
			ElseBlock:        elseBlock,
		}
	} else if t.Type == token.WHILE {
		// WhileStatement
		p.current++
		condition := p.parseExpression(true)
		p.expect(token.LEFT_BRACE, "Expect block after while condition.")
		p.current--
		block := p.parseBlock()
		return &ast.WhileStatement{
			WhileToken: t,
			Condition:  condition,
			Block:      *block,
		}
	} else if t.Type == token.RETURN {
		// ReturnStatement
		p.current++
		var expr ast.Expression
		if p.peek(0).Type != token.SEMICOLON {
			expr = p.parseExpression(false)
		}
		p.expect(token.SEMICOLON, "Expect semicolon after return statement.")
		return &ast.ReturnStatement{
			Expression:  expr,
			ReturnToken: t,
		}
	} else if t.Type == token.PRINT {
		// PrintStatement
		p.current++
		exprs := []ast.Expression{}
		first := p.parseExpression(false)
		exprs = append(exprs, first)
		for p.peek(0).Type == token.COMMA {
			p.current++
			exprs = append(exprs, p.parseExpression(false))
		}
		p.expect(token.SEMICOLON, "Expect semicolon after print statement.")
		return &ast.PrintStatement{
			Expressions: exprs,
			PrintToken:  t,
		}
	} else if t.Type == token.IMPORT {
		// ImportStatement
		p.current++
		pathToken := p.expect(token.STRING, "Expect path to import from.")

		absPath, err := filepath.Abs(pathToken.Lexeme)
		if err != nil {
			panic(err.Error())
		}
		pathToken.Lexeme = absPath

		var alias *token.Token
		if p.peek(0).Type == token.SEMICOLON {
			alias = nil
		} else {
			ident := p.expect(token.IDENTIFIER, "Only identifier is allowed after import path.")
			alias = &ident
		}

		p.expect(token.SEMICOLON, "Expect semicolon after import statement.")

		return &ast.ImportStatement{
			PathToken:  pathToken,
			Identifier: alias,
		}
	} else if t.Type == token.EXPORT {
		// Exported Declaration
		p.current++
		decl := p.parseStatement()

		if structDecl, ok := decl.(*ast.StructDeclaration); ok {
			structDecl.Exported = true
			return structDecl
		} else if funDecl, ok := decl.(*ast.FunctionDeclaration); ok {
			funDecl.Exported = true
			return funDecl
		}

		p.parseError(p.peek(0), "Cannot export non-declaration statement.")
	} else if t.Type == token.LEFT_BRACE {
		// BlockStatement
		return p.parseBlock()
	}

	expr := p.parseExpression(false)
	p.expect(token.SEMICOLON, "Expect semicolon after top-level expression.")
	// ExpressionStatement
	return &ast.ExpressionStatement{
		Expression: expr,
	}
}

func (p *Parser) parseBlock() *ast.BlockStatement {
	p.current++ // skip the `{`
	statements := []ast.Statement{}
	for p.peek(0).Type != token.RIGHT_BRACE {
		if p.peek(0).Type == token.EOF {
			p.parseError(p.peek(0), "Unclosed block.")
		}
		statements = append(statements, p.parseStatement())
	}
	p.current++ // skip the `}`
	if p.peek(0).Type == token.SEMICOLON {
		p.current++ // optional trailing semicolon is allowed
	}
	return &ast.BlockStatement{
		Statements: statements,
	}
}

func (p *Parser) parseExpression(expectingBlock bool) ast.Expression {
	return p.parsePrecedenceExpression(p.parsePrimary(expectingBlock), 0, expectingBlock)
}

type associativity int

const (
	ltr associativity = iota
	rtl
)

type opInfo struct {
	precedence    int
	associativity associativity
}

var operatorPrecedenceMap = map[token.TokenType]opInfo{
	token.PERCENT: {precedence: 6, associativity: ltr},
	token.STAR:    {precedence: 6, associativity: ltr},
	token.SLASH:   {precedence: 6, associativity: ltr},

	token.PLUS:  {precedence: 5, associativity: ltr},
	token.MINUS: {precedence: 5, associativity: ltr},

	token.LESSER:        {precedence: 4, associativity: ltr},
	token.LESSER_EQUAL:  {precedence: 4, associativity: ltr},
	token.GREATER:       {precedence: 4, associativity: ltr},
	token.GREATER_EQUAL: {precedence: 4, associativity: ltr},

	token.EQUAL_EQUAL: {precedence: 3, associativity: ltr},
	token.BANG_EQUAL:  {precedence: 3, associativity: ltr},

	token.AND_AND: {precedence: 2, associativity: ltr},
	token.OR_OR:   {precedence: 2, associativity: ltr},

	token.EQUAL: {precedence: 1, associativity: rtl},
}

func (p *Parser) parsePrecedenceExpression(lhs ast.Expression, minPrecedence int, expectingBlock bool) ast.Expression {
	lookahead := p.peek(0)
	for lookahead.Type.IsBinaryOperator() &&
		operatorPrecedenceMap[lookahead.Type].precedence >= minPrecedence {

		op := lookahead
		p.current++
		rhs := p.parsePrimary(expectingBlock)
		lookahead = p.peek(0)

		// TODO: Clean this up. This is pretty difficult to read.
		for lookahead.Type.IsBinaryOperator() &&
			(operatorPrecedenceMap[lookahead.Type].associativity == ltr &&
				operatorPrecedenceMap[lookahead.Type].precedence >
					operatorPrecedenceMap[op.Type].precedence) ||
			(operatorPrecedenceMap[lookahead.Type].associativity == rtl &&
				operatorPrecedenceMap[lookahead.Type].precedence ==
					operatorPrecedenceMap[op.Type].precedence) {

			rhs = p.parsePrecedenceExpression(rhs, minPrecedence+1, expectingBlock)
			lookahead = p.peek(0)
		}

		lhs = &ast.BinaryExpression{Left: lhs, Operator: op, Right: rhs}
	}

	return lhs
}

func (p *Parser) parseComposite() *ast.CompositeLiteral {
	typ := p.parseType()
	leftBraceToken := p.expect(token.LEFT_BRACE, "Expect `{` after type in composite literal.")

	if p.peek(0).Type == token.RIGHT_BRACE {
		return &ast.CompositeLiteral{
			Typ:                 typ,
			NamedInitializers:   nil,
			UnnamedInitializers: nil,
			LeftBraceToken:      leftBraceToken,
		}
	}

	var (
		namedInitializers      []ast.NamedInitializer
		unnamedInitializers    []ast.Expression
		usingNamedInitializers = false
	)

	if p.peek(0).Type == token.IDENTIFIER {
		p.current++
		if p.peek(0).Type == token.COLON {
			usingNamedInitializers = true

			name := p.peek(-1)
			p.current++ // skip the ':'
			expr := p.parseExpression(false)

			namedInitializers = append(namedInitializers, ast.NamedInitializer{
				Identifier: name,
				Value:      expr,
			})
		} else {
			usingNamedInitializers = false
			unnamedInitializers = append(
				unnamedInitializers,
				&ast.VariableExpression{Identifier: p.peek(-1)},
			)
		}
	} else {
		usingNamedInitializers = false
		expr := p.parseExpression(false)
		unnamedInitializers = append(unnamedInitializers, expr)
	}

	for p.peek(0).Type == token.COMMA {
		p.current++

		if p.peek(0).Type == token.RIGHT_BRACE {
			break
		} else if usingNamedInitializers {
			name := p.expect(token.IDENTIFIER, "Expect identifier for initializer in composite literal.")
			p.expect(token.COLON, "Expect `:` in composite literal initializer.")
			expr := p.parseExpression(false)
			namedInitializers = append(namedInitializers, ast.NamedInitializer{
				Identifier: name,
				Value:      expr,
			})
		} else {
			expr := p.parseExpression(false)
			unnamedInitializers = append(unnamedInitializers, expr)
		}
	}

	if p.peek(0).Type == token.COMMA {
		p.current++ // optional trailing comma is skipped
	}

	p.expect(token.RIGHT_BRACE, "Expect `}` after initializers in compositer literal.")

	if usingNamedInitializers {
		return &ast.CompositeLiteral{
			Typ:                 typ,
			NamedInitializers:   &namedInitializers,
			UnnamedInitializers: nil,
			LeftBraceToken:      leftBraceToken,
		}
	} else {
		return &ast.CompositeLiteral{
			Typ:                 typ,
			NamedInitializers:   nil,
			UnnamedInitializers: &unnamedInitializers,
			LeftBraceToken:      leftBraceToken,
		}
	}
}

func (p *Parser) parsePrimary(expectingBlock bool) ast.Expression {
	var expr ast.Expression

	if p.peek(0).Type == token.INT ||
		p.peek(0).Type == token.FLOAT ||
		p.peek(0).Type == token.STRING ||
		p.peek(0).Type == token.TRUE ||
		p.peek(0).Type == token.FALSE {
		p.current++
		expr = &ast.Literal{
			Token:        p.peek(-1),
			LiteralValue: p.peek(-1).Lexeme,
		}
	} else if p.peek(0).Type == token.LEFT_PAREN {
		p.parseError(p.peek(0), "Grouping expressions are not yet supported.")
	} else if p.peek(0).Type == token.LEFT_BRACKET {
		lbtoken := p.peek(0)
		p.current++
		expressions := []ast.Expression{}

		if p.peek(0).Type != token.RIGHT_BRACKET {
			for {
				expressions = append(expressions, p.parseExpression(false))

				if p.peek(0).Type != token.COMMA {
					break
				} else {
					p.current++ // skip the comma
				}
			}
		}

		p.expect(token.RIGHT_BRACKET, "Expect closing bracket in slice literal.")

		expr = &ast.SliceLiteral{
			Expressions:      expressions,
			LeftBracketToken: lbtoken,
		}
	} else if p.peek(0).Type == token.IDENTIFIER {
		p.current++
		if p.peek(0).Type == token.LEFT_BRACE && !expectingBlock {
			//                                 ---------------
			// Since parsing a composite literal can be ambiguous between keywords and blocks
			// e.g. between the `if`/`while` keyword and block.
			//
			// The Go specification mentions this in https://golang.org/ref/spec#Composite_literals
			p.current--
			expr = p.parseComposite()
		} else {
			expr = &ast.VariableExpression{
				Identifier: p.peek(-1),
			}
		}
	} else if p.peek(0).Type == token.MINUS || p.peek(0).Type == token.BANG {
		p.current++
		expr = &ast.UnaryExpression{
			Operator: p.peek(-1),
			Value:    p.parsePrimary(false),
		}
	} else if p.peek(0).Type == token.AND {
		andToken := p.peek(0)
		p.current++
		expr = &ast.ReferenceOf{
			Target:   p.parsePrimary(false),
			AndToken: andToken,
		}
	}

	for p.peek(0).Type == token.LEFT_PAREN ||
		p.peek(0).Type == token.LEFT_BRACKET ||
		p.peek(0).Type == token.DOT ||
		p.peek(0).Type == token.AS ||
		p.peek(0).Type == token.IS ||
		p.peek(0).Type == token.CARET {

		if p.peek(0).Type == token.LEFT_PAREN {
			leftParenToken := p.peek(0)

			p.current++
			arguments := []ast.Expression{}

			if p.peek(0).Type != token.RIGHT_PAREN {
				for {
					arguments = append(arguments, p.parseExpression(false))

					if p.peek(0).Type != token.COMMA {
						break
					} else {
						p.current++ // skip the comma
					}
				}
			}

			p.expect(token.RIGHT_PAREN, "Missing closing parenthesis in function call.")
			expr = &ast.CallExpression{
				Callee:         expr,
				Arguments:      arguments,
				LeftParenToken: leftParenToken,
			}
		}

		if p.peek(0).Type == token.LEFT_BRACKET {
			leftBracketToken := p.peek(0)
			p.current++
			index := p.parseExpression(false)

			p.expect(token.RIGHT_BRACKET, "Missing close bracket in index expression.")
			expr = &ast.IndexExpression{
				Expression:       expr,
				Index:            index,
				LeftBracketToken: leftBracketToken,
			}
		}

		if p.peek(0).Type == token.DOT {
			p.current++
			ident := p.expect(token.IDENTIFIER, "Expect identifier after `.` in get expression.")
			expr = &ast.GetExpression{
				Expression: expr,
				Identifier: ident,
			}
		}

		if p.peek(0).Type == token.CARET {
			p.current++
			expr = &ast.Dereference{
				Expression: expr,
				CaretToken: p.peek(-1),
			}
		}

		if p.peek(0).Type == token.AS {
			asToken := p.peek(0)
			p.current++
			expr = &ast.AsExpression{
				Expression: expr,
				TargetType: p.parseType(),
				AsToken:    asToken,
			}
		}

		if p.peek(0).Type == token.IS {
			isToken := p.peek(0)
			p.current++
			expr = &ast.IsExpression{
				Expression:   expr,
				ComparedType: p.parseType(),
				IsToken:      isToken,
			}
		}
	}

	if expr == nil {
		p.parseError(p.peek(0), "Expected expression.")
	}

	return expr
}

func (p *Parser) parseType() ast.Type {
	var typ ast.Type

	if p.peek(0).Type == token.IDENTIFIER {
		p.current++

		if isIdentifierPrimitive(p.peek(-1)) {
			typ = &ast.Primitive{
				Name: p.peek(-1).Lexeme,
			}
		} else {
			typ = &ast.StructType{
				Name: p.peek(-1).Lexeme,
				// Members are resolved in analysis
			}
		}
	} else if p.peek(0).Type == token.STAR {
		p.current++
		typ = &ast.PointerType{
			ElType: p.parseType(),
		}
	} else if p.peek(0).Type == token.TILDE {
		p.current++
		typ = &ast.BoxType{
			ElType: p.parseType(),
		}
	} else if p.peek(0).Type == token.LEFT_BRACKET {
		p.current++
		elType := p.parseType()
		p.expect(token.RIGHT_BRACKET, "Expect closing bracket in slice type.")
		typ = &ast.SliceType{
			ElType: elType,
		}
	} else if p.peek(0).Type == token.LEFT_PAREN {
		// Type grouping: useful to show precedence when creating sum types.
		p.current++
		typ = p.parseType()
		p.expect(token.RIGHT_PAREN, "Unclosed type grouping.")
	}

	for p.peek(0).Type == token.OR {
		p.current++
		if sumType, ok := typ.(*ast.SumType); ok {
			sumType.Options = append(sumType.Options, p.parseType())
		} else {
			sumType := ast.SumType{Options: []ast.Type{typ}}
			sumType.Options = append(sumType.Options, p.parseType())
			typ = &sumType
		}
	}

	// Flatten any sum-types
	if s, ok := typ.(*ast.SumType); ok {
		p.flattenSumType(s)
	}

	if typ == nil {
		p.parseError(
			p.peek(0),
			fmt.Sprintf(
				"Expected type, found token: '%s' instead.",
				p.peek(0).Lexeme,
			),
		)
	}

	return typ
}

func (p *Parser) flattenSumType(s *ast.SumType) {
	var newOptions []ast.Type

	for _, o := range s.Options {
		if nested, ok := o.(*ast.SumType); ok {
			p.flattenSumType(nested)
			newOptions = append(newOptions, nested.Options...)
		} else {
			if typeSliceContains(newOptions, o) {
				p.parseError(p.peek(0), fmt.Sprintf(
					"Found duplicate type: '%s' in sum type.", o.String(),
				))
			}

			newOptions = append(newOptions, o)
		}
	}

	s.Options = newOptions
}

func typeSliceContains(s []ast.Type, o ast.Type) bool {
	for _, option := range s {
		if option.Equals(o) {
			return true
		}
	}

	return false
}

func Parse(m *ast.Module) {
	p := Parser{Module: m}

	result := []ast.Statement{}

	for p.peek(0).Type != token.EOF {
		result = append(result, p.parseStatement())
	}

	m.Statements = result
}
