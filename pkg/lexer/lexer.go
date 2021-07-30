package lexer

import (
	"fmt"
	"log"
	"unicode"

	"github.com/kartiknair/myc/pkg/ast"
	"github.com/kartiknair/myc/pkg/token"
)

type Lexer struct {
	start     int
	current   int
	line      int
	lineBegin int
	tokens    []token.Token

	Module *ast.Module
}

func lexError(line int, message string) {
	log.Fatalf("lex-error: %d: %s", line, message)
}

func (l *Lexer) isAtEnd() bool {
	return l.current >= len(l.Module.Source)
}

func (l *Lexer) advance() byte {
	l.current++
	return l.Module.Source[l.current-1]
}

func (l *Lexer) match(c byte) bool {
	if l.isAtEnd() {
		return false
	} else if l.Module.Source[l.current] == c {
		l.current++
		return true
	} else {
		return false
	}
}

func (l *Lexer) peek(distanceOptionalShim ...int) byte {
	distance := 0

	if len(distanceOptionalShim) > 0 {
		distance = distanceOptionalShim[0]
	}

	if l.isAtEnd() {
		lexError(l.line, "Unexpected end of file.")
	}

	return l.Module.Source[l.current+distance]
}

func (l *Lexer) addToken(typ token.TokenType, lexeme string) {
	if lexeme == "" {
		lexeme = l.Module.Source[l.start:l.current]
	}

	l.tokens = append(l.tokens, token.Token{
		Lexeme: lexeme,
		Type:   typ,
		Pos:    token.Pos{Line: l.line, Column: l.current - l.lineBegin},
	})
}

func isDigit(b byte) bool {
	return unicode.IsDigit(rune(b))
}

func isAlphaNumeric(b byte) bool {
	return unicode.IsDigit(rune(b)) || unicode.IsLetter(rune(b))
}

func (l *Lexer) lexString() {
	for l.peek() != '"' && !l.isAtEnd() {
		if l.peek() == '\n' {
			// TODO: Multi-line strings wouldn't be too difficult to implement
			lexError(l.line, "Strings must be on a single line.")
		}

		l.advance()
	}

	if l.isAtEnd() {
		lexError(l.line, "Unterminated string literal.")
	}

	l.advance()

	value := l.Module.Source[l.start+1 : l.current-1]
	l.addToken(token.STRING, value)
}

func (l *Lexer) lexNumber() {
	t := token.INT

	for isDigit(l.peek()) {
		l.advance()
	}

	// Look for a fractional part.
	if l.peek() == '.' && isDigit(l.peek(1)) {
		t = token.FLOAT

		// Consume the "."
		l.advance()

		for isDigit(l.peek()) {
			l.advance()
		}
	}

	l.addToken(t, "")
}

func (l *Lexer) lexIdent() {
	for isAlphaNumeric(l.peek()) {
		l.advance()
	}

	text := l.Module.Source[l.start:l.current]

	for i, kw := range token.Keywords {
		if kw == text {
			l.addToken(token.TokenType(int(token.KEYWORD_BEGIN)+i+1), text)
			return
		}
	}

	l.addToken(token.IDENTIFIER, text)
}

func (l *Lexer) ScanToken() {
	c := l.advance()

	switch c {
	case '(':
		l.addToken(token.LEFT_PAREN, "")
	case ')':
		l.addToken(token.RIGHT_PAREN, "")
	case '{':
		l.addToken(token.LEFT_BRACE, "")
	case '}':
		l.addToken(token.RIGHT_BRACE, "")
	case '[':
		l.addToken(token.LEFT_BRACKET, "")
	case ']':
		l.addToken(token.RIGHT_BRACKET, "")
	case ',':
		l.addToken(token.COMMA, "")
	case '.':
		l.addToken(token.DOT, "")
	case ':':
		l.addToken(token.COLON, "")
	case '~':
		l.addToken(token.TILDE, "")
	case '&':
		if l.match('&') {
			l.addToken(token.AND_AND, "")
		} else {
			l.addToken(token.AND, "")
		}
	case '|':
		if l.match('|') {
			l.addToken(token.OR_OR, "")
		} else {
			l.addToken(token.OR, "")
		}
	case '!':
		if l.match('=') {
			l.addToken(token.BANG_EQUAL, "")
		} else {
			l.addToken(token.BANG, "")
		}
	case '<':
		if l.match('=') {
			l.addToken(token.LESSER_EQUAL, "")
		} else {
			l.addToken(token.LESSER, "")
		}
	case '>':
		if l.match('=') {
			l.addToken(token.GREATER_EQUAL, "")
		} else {
			l.addToken(token.GREATER, "")
		}
	case '^':
		l.addToken(token.CARET, "")
	case '-':
		l.addToken(token.MINUS, "")
	case '+':
		l.addToken(token.PLUS, "")
	case '*':
		l.addToken(token.STAR, "")
	case '%':
		l.addToken(token.PERCENT, "")
	case '=':
		if l.match('=') {
			l.addToken(token.EQUAL_EQUAL, "")
		} else {
			l.addToken(token.EQUAL, "")
		}
	case '/':
		if l.match('/') {
			// a comment goes until the end of the line.
			for l.peek() != '\n' && !l.isAtEnd() {
				l.advance()
			}
		} else if unicode.IsLetter(rune(c)) {
			l.lexIdent()
		} else {
			l.addToken(token.SLASH, "")
		}
	case ' ':
	case '\r':
	case '\t':
		// ignore whitespace.
	case ';':
		l.addToken(token.SEMICOLON, ";")
	case '\n':
		// “if the newline comes after a token that could end a
		// statement, insert a semicolon”.
		// Source: https://golang.org/doc/effective_go#semicolons

		if len(l.tokens) > 0 {
			currentTokensType := l.tokens[len(l.tokens)-1].Type

			if currentTokensType != token.SEMICOLON &&
				(currentTokensType == token.IDENTIFIER ||
					currentTokensType == token.RIGHT_PAREN ||
					currentTokensType == token.RIGHT_BRACE ||
					currentTokensType == token.RIGHT_BRACKET ||
					currentTokensType == token.RETURN ||
					currentTokensType == token.INT ||
					currentTokensType == token.FLOAT ||
					currentTokensType == token.TRUE ||
					currentTokensType == token.FALSE ||
					currentTokensType == token.CARET ||
					currentTokensType == token.STRING) {
				l.addToken(token.SEMICOLON, "")
			}
		}

		l.line++
		l.lineBegin = l.current
	case '"':
		l.lexString()
	default:
		if isDigit(c) {
			l.lexNumber()
		} else if unicode.IsLetter((rune(c))) {
			l.lexIdent()
		} else {
			lexError(l.line, fmt.Sprintf("Unexpected character: %c", c))
		}
	}
}

func Lex(m *ast.Module) {
	l := Lexer{Module: m}

	for !l.isAtEnd() {
		// we are at the beginning of the next lexeme.
		l.start = l.current
		l.ScanToken()
	}

	l.addToken(token.EOF, "\x00")
	m.Tokens = l.tokens
}
