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
var n1 = 24
var n2 = 32

if n1 < 20 && n2 < 20 {
	print n1, n2
} else if n1 < 20 {
	print n1
} else {
	print n2
}

while n1 > 0 {
	print n1
	n1 = n1 - 1
}
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(parsed)
	fmt.Println(gennedLLVM)
}
