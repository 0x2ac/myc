package gen

import (
	"github.com/kartiknair/myc/ast"
	cgen "github.com/kartiknair/myc/gen/c"
	llvmgen "github.com/kartiknair/myc/gen/llvm"
)

func C(statements []ast.Statement) string {
	return cgen.Gen(statements)
}

func LLVM(statements []ast.Statement) string {
	return llvmgen.Gen(statements)
}
