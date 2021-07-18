package main

import (
	"github.com/kartiknair/myc/analyzer"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
)

func main() {
	code := `
struct Container {
	value int
}

fun makeContainer() Container {
	var s int|str = 24
	return Container{value: s}
}

fun main() {
	var c = makeContainer()
	print c
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
