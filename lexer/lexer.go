package lexer

import (
	"fmt"
	"log"
	"strings"
	"unicode"
)

var Keywords = [...]string{
	"var",
	"print",
	"fun",
	"return",
	"struct",
	"true",
	"false",
	"if",
	"else",
	"while",
	"as",
	"is",
}

type Pos struct {
	Line   int
	Column int
}

func (t *Token) SourceContext() string {
	source = strings.ReplaceAll(source, "\r\n", "\n")
	sourceLines := strings.Split(source, "\n")
	numLines := len(sourceLines)

	var highlightChar byte = '^'
	offsetHighlight := make([]byte, t.Pos.Column)

	for i := 0; i < t.Pos.Column; i++ {
		if sourceLines[t.Pos.Line-1][i] == '\t' {
			offsetHighlight[i] = '\t'
		} else {
			offsetHighlight[i] = ' '
		}
	}

	offsetHighlight[t.Pos.Column-1] = highlightChar

	if t.Pos.Line == 1 {
		return fmt.Sprintf(`
%4d | %s
     | %s`,
			1,
			sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
		)
	} else if t.Pos.Line == numLines-1 {
		return fmt.Sprintf(`
%4d | %s
%4d | %s
     | %s`,
			t.Pos.Line-1, sourceLines[t.Pos.Line-2],
			t.Pos.Line, sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
		)

	} else {
		return fmt.Sprintf(`
%4d | %s
%4d | %s
     | %s
%4d | %s`,
			t.Pos.Line-1, sourceLines[t.Pos.Line-2],
			t.Pos.Line, sourceLines[t.Pos.Line-1],
			string(offsetHighlight),
			t.Pos.Line+1, sourceLines[t.Pos.Line],
		)
	}
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
	PRINT
	FUN
	RETURN
	STRUCT
	TRUE
	FALSE
	IF
	ELSE
	WHILE
	AS
	IS
	keyword_end

	LEFT_PAREN
	RIGHT_PAREN
	LEFT_BRACE
	RIGHT_BRACE
	LEFT_BRACKET
	RIGHT_BRACKET
	COMMA
	DOT
	COLON
	CARET
	AND
	OR
	BANG
	TILDE
	INTERP_BEGIN
	INTERP_END

	binaryop_begin
	EQUAL
	PLUS
	MINUS
	STAR
	SLASH
	PERCENT

	LESSER
	GREATER
	LESSER_EQUAL
	GREATER_EQUAL
	EQUAL_EQUAL
	BANG_EQUAL

	AND_AND
	OR_OR
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
	log.Fatalf("lex-error: %d: %s", line, message)
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

	if isAtEnd() {
		lexError(line, "Unexpected end of file.")
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
			// TODO: Multi-line strings wouldn't be too difficult to implement
			lexError(line, "Strings must be on a single line.")
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
	case '[':
		addToken(LEFT_BRACKET, "")
	case ']':
		addToken(RIGHT_BRACKET, "")
	case ',':
		addToken(COMMA, "")
	case '.':
		addToken(DOT, "")
	case ':':
		addToken(COLON, "")
	case '~':
		addToken(TILDE, "")
	case '&':
		if match('&') {
			addToken(AND_AND, "")
		} else {
			addToken(AND, "")
		}
	case '|':
		if match('|') {
			addToken(OR_OR, "")
		} else {
			addToken(OR, "")
		}
	case '!':
		if match('=') {
			addToken(BANG_EQUAL, "")
		} else {
			addToken(BANG, "")
		}
	case '<':
		if match('=') {
			addToken(LESSER_EQUAL, "")
		} else {
			addToken(LESSER, "")
		}
	case '>':
		if match('=') {
			addToken(GREATER_EQUAL, "")
		} else {
			addToken(GREATER, "")
		}
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
		if match('=') {
			addToken(EQUAL_EQUAL, "")
		} else {
			addToken(EQUAL, "")
		}
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
					currentTokensType == RIGHT_BRACKET ||
					currentTokensType == RETURN ||
					currentTokensType == INT ||
					currentTokensType == FLOAT ||
					currentTokensType == TRUE ||
					currentTokensType == FALSE ||
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
