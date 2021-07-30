package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/kartiknair/myc/pkg/analyzer"
	"github.com/kartiknair/myc/pkg/ast"
	"github.com/kartiknair/myc/pkg/gen"
	"github.com/kartiknair/myc/pkg/lexer"
	"github.com/kartiknair/myc/pkg/parser"
)

func main() {
	code := `
fun sum(a int, b int) int {
	return a + b
}

fun fib(n int) int {
	if n < 2 {
		return n
	}

	return fib(n-1) + fib(n-2)
}

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
	print fib(20)
	retEarly()
}`
	cwd, err := os.Getwd()
	if err != nil {
		log.Fatal(err.Error())
	}

	m := ast.Module{
		Path:    filepath.Join(cwd, "main.myc"),
		Source:  code,
		Exports: make(map[string]ast.Type),
	}

	lexer.Lex(&m)
	parser.Parse(&m)
	analyzer.Analyze(&m)

	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(&m)
	fmt.Println(gennedLLVM)
}
