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
fun retEarly() {
	var i = 0
	while true {
		if i == 10 {
			return
		}
		
		print i
		i = i + 1
	}
}

fun main() {
	retEarly()
}
`
	tokens := lexer.Lex(code)
	// repr.Println(tokens)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(parsed)
	fmt.Println(gennedLLVM)
}
