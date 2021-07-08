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
var s1 = "some string"
var s2 = "some other string"
var s3 = s1 + " " + s2
var s4 = "s1: '#{s1}', s2: '#{s2}'"

print s1
print s2
print s3
print s4

var n1 = 23
var n2 = 34.56

print "n1: #{n1}, n2: #{n2}"
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
