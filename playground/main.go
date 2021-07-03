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
fun inc(p *int) {
	p^ = p^ + 1
}

var n = &42
print n^
inc(n)
print n^
`
	tokens := lexer.Lex(code)
	parsed := parser.Parse(tokens)
	analyzer.Analyze(parsed)
	gennedC := gen.C(parsed)
	fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(parsed)
	fmt.Println(gennedLLVM)
}
