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
import "./examples/modules/math.myc"

// fun incPoint(p math.Point) math.Point {
// 	return math.Point{
// 		x: p.x+1.0,
// 		y: p.y+1.0,
// 	}
// }

fun main() {
	var p1 = math.Point{x: 34.0, y: 56.0}
	print p1
	// var p2 = incPoint(p1)
	// print p2
}
`
	cwd, err := os.Getwd()
	if err != nil {
		log.Fatal(err.Error())
	}

	m := ast.Module{
		Name:    "",
		Path:    filepath.Join(cwd, "main.myc"),
		Source:  code,
		Imports: make(map[string]*ast.Module),
		Exports: make(map[string]ast.Type),
	}

	lexer.Lex(&m)
	parser.Parse(&m)
	// repr.Println(m.Statements)
	analyzer.Analyze(&m)

	// gennedC := gen.C(parsed)
	// fmt.Println(gennedC)
	gennedLLVM := gen.LLVM(&m)
	fmt.Println(gennedLLVM)
	// fmt.Println(llvmgen.GenRuntime())
}
