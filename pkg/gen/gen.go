package gen

import (
	"github.com/kartiknair/myc/pkg/ast"
	cgen "github.com/kartiknair/myc/pkg/gen/c"
	llvmgen "github.com/kartiknair/myc/pkg/gen/llvm"
)

func C(m *ast.Module) string {
	return cgen.Gen(m)
}

func LLVM(m *ast.Module) string {
	return llvmgen.Gen(m)
}
