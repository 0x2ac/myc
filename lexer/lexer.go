package lexer

import (
	"fmt"
	"log"
	"unicode"
)

var Keywords = [...]string{
	"var",
	"const",
	"print",
	"fun",
	"return",
	"struct",
}

type Pos struct {
	Line   int
	Column int
}

type TokenType int

const (
	STRING TokenType = iota
	INT
	FLOAT
	IDENTIFIER
	EOF

	keyword_begin
	VAR
	CONST
	PRINT
	FUN
	RETURN
	STRUCT
	keyword_end

	LEFT_PAREN
	RIGHT_PAREN
	LEFT_BRACE
	RIGHT_BRACE
	COMMA
	DOT
	COLON
	CARET
	AND

	binaryop_begin
	EQUAL
	PLUS
	MINUS
	STAR
	SLASH
	PERCENT
	binaryop_end

	SEMICOLON
)

func (t TokenType) IsBinaryOperator() bool {
	return t > binaryop_begin && t < binaryop_end
}

type Token struct {
	Lexeme string
	Type   TokenType
	Pos    Pos
}

func lexError(line int, message string) {
	log.Fatalf("[Lexer Error on line: %d] %s", line, message)
}

var (
	start     int
	current   int
	line      int = 1
	lineBegin int
	tokens    []Token
	source    string
)

func isAtEnd() bool {
	return current >= len(source)
}

func advance() byte {
	current++
	return source[current-1]
}

func match(c byte) bool {
	if isAtEnd() {
		return false
	} else if source[current] == c {
		current++
		return true
	} else {
		return false
	}
}

func peek(distanceOptionalShim ...int) byte {
	distance := 0

	if len(distanceOptionalShim) > 0 {
		distance = distanceOptionalShim[0]
	}

	return source[current+distance]
}

func addToken(typ TokenType, lexeme string) {
	if lexeme == "" {
		lexeme = source[start:current]
	}

	tokens = append(tokens, Token{
		Lexeme: lexeme,
		Type:   typ,
		Pos:    Pos{Line: line, Column: current - lineBegin},
	})
}

func isDigit(b byte) bool {
	return unicode.IsDigit(rune(b))
}

func isAlphaNumeric(b byte) bool {
	return unicode.IsDigit(rune(b)) || unicode.IsLetter(rune(b))
}

func lexString() {
	for peek() != '"' && !isAtEnd() {
		if peek() == '\n' {
			line++
			lineBegin = current
		}

		advance()
	}

	if isAtEnd() {
		lexError(line, "Unterminated string literal.")
	}

	advance()

	value := source[start+1 : current-1]
	addToken(STRING, value)
}

func lexNumber() {
	t := INT

	for isDigit(peek()) {
		advance()
	}

	// Look for a fractional part.
	if peek() == '.' && isDigit(peek(1)) {
		t = FLOAT

		// Consume the "."
		advance()

		for isDigit(peek()) {
			advance()
		}
	}

	addToken(t, "")
}

func lexIdent() {
	for isAlphaNumeric(peek()) {
		advance()
	}

	text := source[start:current]

	for i, kw := range Keywords {
		if kw == text {
			addToken(TokenType(int(keyword_begin)+i+1), text)
			return
		}
	}

	addToken(IDENTIFIER, text)
}

func ScanToken() {
	c := advance()

	switch c {
	case '(':
		addToken(LEFT_PAREN, "")
	case ')':
		addToken(RIGHT_PAREN, "")
	case '{':
		addToken(LEFT_BRACE, "")
	case '}':
		addToken(RIGHT_BRACE, "")
	case ',':
		addToken(COMMA, "")
	case '.':
		addToken(DOT, "")
	case ':':
		addToken(COLON, "")
	case '&':
		addToken(AND, "")
	case '^':
		addToken(CARET, "")
	case '-':
		addToken(MINUS, "")
	case '+':
		addToken(PLUS, "")
	case '*':
		addToken(STAR, "")
	case '%':
		addToken(PERCENT, "")
	case '=':
		addToken(EQUAL, "")
	case '/':
		if match('/') {
			// a comment goes until the end of the line.
			for peek() != '\n' && !isAtEnd() {
				advance()
			}
		} else if unicode.IsLetter(rune(c)) {
			lexIdent()
		} else {
			addToken(SLASH, "")
		}
	case ' ':
	case '\r':
	case '\t':
		// ignore whitespace.
	case ';':
		addToken(SEMICOLON, ";")
	case '\n':
		// “if the newline comes after a token that could end a
		// statement, insert a semicolon”.
		// Source: https://golang.org/doc/effective_go#semicolons

		if len(tokens) > 0 {
			currentTokensType := tokens[len(tokens)-1].Type

			if currentTokensType != SEMICOLON &&
				(currentTokensType == IDENTIFIER ||
					currentTokensType == RIGHT_PAREN ||
					currentTokensType == RIGHT_BRACE ||
					currentTokensType == RETURN ||
					currentTokensType == INT ||
					currentTokensType == FLOAT ||
					currentTokensType == CARET ||
					currentTokensType == STRING) {
				addToken(SEMICOLON, "")
			}
		}

		line++
		lineBegin = current
	case '"':
		lexString()
	default:
		if isDigit(c) {
			lexNumber()
		} else if unicode.IsLetter((rune(c))) {
			lexIdent()
		} else {
			lexError(line, fmt.Sprintf("Unexpected character: %c", c))
		}
	}
}

func Lex(code string) []Token {
	source = code

	for !isAtEnd() {
		// we are at the beginning of the next lexeme.
		start = current
		ScanToken()
	}

	addToken(EOF, "\x00")
	return tokens
}
