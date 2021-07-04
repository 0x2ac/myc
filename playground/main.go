package main

import (
	"github.com/kartiknair/myc/analyzer"
	"github.com/kartiknair/myc/lexer"
	"github.com/kartiknair/myc/parser"
)

func main() {
	code := `
var a = true && false
var b = 23 == 23 && 43 == 45

print a
print b

fun equal(a int, b int) bool {
    return a == b
}

fun test() bool {
	return false
}

fun and(x bool, y bool) bool {
	return x && y
}

print a && b
print and(a, b)
print equal(23, 42)
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	// gennedLLVM := gen.LLVM(parsed)
	// fmt.Println(gennedLLVM)
}
