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
struct Point {
	x int
	y int
}

var p1 = Point{1, 4}
var p2 = Point{x: 3, y: 4}
var p3 = Point{
	x: 56,
	y: 78,
}

print p1
print p2.x + p3.x
print p2.y + p3.y
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	gennedC := gen.C(parsed)
	fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(parsed)
	fmt.Println(gennedLLVM)
}
