package token

type TokenType int

const (
	STRING TokenType = iota
	INT
	FLOAT
	IDENTIFIER
	EOF

	KEYWORD_BEGIN
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
	IMPORT
	EXPORT
	KEYWORD_END

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

func (t TokenType) IsComparativeOperator() bool {
	return t >= LESSER && t <= OR_OR
}

type Token struct {
	Lexeme string
	Type   TokenType
	Pos    Pos
}

type Pos struct {
	Line   int
	Column int
}

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
	"import",
	"export",
}
