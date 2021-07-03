package main

import (
	"github.com/kartiknair/myc/analyzer"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
)

func main() {
	code := `
var n = 42
var p = &n
print p
print p^
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	// gennedLLVM := gen.LLVM(parsed)
	// fmt.Println(gennedLLVM)
}
