package main

import (
	"fmt"

	"github.com/kartiknair/myc/analyzer"
	"github.com/kartiknair/myc/gen"
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
	gennedC := gen.C(parsed)
	fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(parsed)
	fmt.Println(gennedLLVM)
}
