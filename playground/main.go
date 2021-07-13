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
struct Container {
	value ~int
}

fun gimmeBox() ~int {
	return 52
}

fun borrowBoxes(a ~int, b ~int) {
	print a
	print b
	print a^ + b^
}

fun main() {
	var c1 = Container{value: 56}
	var c2 = Container{value: 32}

	c1.value = c2.value^

	var b1 ~int = 45
	var b2 ~int = 32

	b1 = gimmeBox()
	borrowBoxes(b1, b2)

	var b ~int = 32
	b = 45

	print b^
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
