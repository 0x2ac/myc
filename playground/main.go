package main

import (
	"github.com/kartiknair/myc/analyzer"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
)

func main() {
	code := `
struct Container {
	value ~int
}

fun main() {
	var c1 = Container{value: 56}
	var c2 = Container{value: 34}
	var cs = [c1, c2]
}
`
	tokens := lexer.Lex(code)
	// repr.Println(tokens)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	// gennedLLVM := gen.LLVM(parsed)
	// fmt.Println(gennedLLVM)
}
