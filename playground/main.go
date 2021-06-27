package main

import (
	"fmt"

	"github.com/kartiknair/myc/analyzer"
	cgen "github.com/kartiknair/myc/gen/c"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
)

func main() {
	code := `
fun sum(a int, b int) int {
	return a + b
}

var total = sum(23, 45)

print total + 1
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	genned := cgen.Gen(parsed)
	fmt.Println(genned)
}
