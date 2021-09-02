package llvmgen

import (
	"fmt"
	"math"
	"strconv"

	"github.com/kartiknair/myc/pkg/ast"
	"github.com/kartiknair/myc/pkg/token"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

var (
	module                     *ir.Module
	block                      *ir.Block
	currentFunctionsDropBlock  *ir.Block
	currentFunctionsReturnType ast.Type
	currentFunctionsReturnPhi  *ir.InstPhi

	putsDeclaration   *ir.Func
	panicDeclaration  *ir.Func
	memcpyDeclaration *ir.Func
	mallocDeclaration *ir.Func
	freeDeclaration   *ir.Func

	namespace = make(map[string]value.Value)
	typespace = make(map[string]types.Type)
)

// Calculates the compile-time size of a type. Packed because it does not
// consider alignment at all, just the elements. Returned value is in bits.
func getPackedSize(t ast.Type) int {
	// TODO: make this more flexible
	const ARCH_SIZE = 64

	switch t := t.(type) {
	case *ast.Primitive:
		switch t.Kind {
		case ast.I8:
			return 8
		case ast.I16:
			return 16
		case ast.I32:
			return 32
		case ast.I64:
			return 64

		case ast.U8:
			return 8
		case ast.U16:
			return 16
		case ast.U32:
			return 32
		case ast.U64:
			return 64

		case ast.F32:
			return 32
		case ast.F64:
			return 64

		case ast.Bool:
			return 1
		case ast.Str:
			return 64 + ARCH_SIZE + 1

		default:
			panic("Attempted to get size of unknown primitive type.")
		}
	case *ast.BoxType:
		return ARCH_SIZE
	case *ast.PointerType:
		return ARCH_SIZE
	case *ast.SliceType:
		// {length u64; capacity u64; buffer *T}
		return 64 + 64 + ARCH_SIZE
	case *ast.StructType:
		total := 0
		for _, m := range t.Members {
			total += getPackedSize(m.Type)
		}
		return total
	case *ast.SumType:
		tagIntSize := math.Floor(math.Log2(float64(len(t.Options)-1))) + 1

		var largestOption ast.Type
		for _, o := range t.Options {
			if largestOption == nil || getPackedSize(o) > getPackedSize(largestOption) {
				largestOption = o
			}
		}

		return int(tagIntSize) + getPackedSize(largestOption)
	}

	panic(fmt.Sprintf("Size calculation has not been implemented for type: `%s` yet.", t))
}

func genSizeOf(t ast.Type) value.Value {
	// Based on this SO answer: https://stackoverflow.com/a/30830445/12785202
	gep := block.NewGetElementPtr(
		genType(t),
		constant.NewNull(types.NewPointer(genType(t))),
		constant.NewInt(types.I32, 1),
	)
	return block.NewPtrToInt(gep, types.I64)
}

func genType(t ast.Type) types.Type {
	switch t.(type) {
	case *ast.Primitive:
		return genPrimitive(t.(*ast.Primitive))
	case *ast.SliceType:
		st := t.(*ast.SliceType)
		// { buffer *ElType, length i64, capacity i64 }
		typ := types.NewStruct(types.NewPointer(genType(st.ElType)), types.I64, types.I64)
		ensureSliceMethods(st, typ)
		return typ
	case *ast.StructType:
		s := t.(*ast.StructType)
		if s.SourceModule.Name != "main" {
			typ, ok := typespace[s.SourceModule.Name+"."+s.Name]
			if !ok {
				panic("Internal error. Could not resolve imported type.")
			}
			return typ
		}
		return typespace[t.(*ast.StructType).Name]
	case *ast.PointerType:
		typ := types.NewPointer(genType(t.(*ast.PointerType).ElType))
		return typ
	case *ast.BoxType:
		typ := types.NewPointer(genType(t.(*ast.BoxType).ElType))
		ensureBoxDrop(t.(*ast.BoxType), typ)
		return typ
	case *ast.SumType:
		st := t.(*ast.SumType)
		// floor(log2(maxNumber)) + 1
		// ^ Gives you the number of bits required to store an
		// unsigned decimal number in binary.
		numBitsForOptions := math.Floor(math.Log2(float64(len(st.Options)-1))) + 1

		indexType := types.NewInt(uint64(numBitsForOptions))
		var largestOption ast.Type
		for _, o := range st.Options {
			if largestOption == nil || getPackedSize(o) > getPackedSize(largestOption) {
				largestOption = o
			}
		}

		typ := types.NewStruct(indexType, genType(largestOption))
		ensureSumTypeDrop(st, typ)
		return typ
	}

	panic("Type node has invalid static type.")
}

func genPrimitive(primitive *ast.Primitive) types.Type {
	switch primitive.Kind {
	case ast.I8:
		return types.I8
	case ast.I16:
		return types.I16
	case ast.I32:
		return types.I32
	case ast.I64:
		return types.I64

	case ast.U8:
		return types.I8
	case ast.U16:
		return types.I16
	case ast.U32:
		return types.I32
	case ast.U64:
		return types.I64

	case ast.F32:
		return types.Float
	case ast.F64:
		return types.Double

	case ast.Bool:
		return types.I1
	case ast.Str:
		return types.NewStruct(types.I8Ptr, types.I64, types.I1)

	default:
		panic("Attempted to generate unknown primitive type.")
	}
}

func getTypeName(t ast.Type) string {
	if p, ok := t.(*ast.Primitive); ok {
		return p.String()
	} else if s, ok := t.(*ast.StructType); ok {
		if s.SourceModule.Name != "main" {
			return s.SourceModule.Name + "." + s.Name
		}
		return s.Name
	} else if st, ok := t.(*ast.SliceType); ok {
		return fmt.Sprintf("slice_%s", getTypeName(st.ElType))
	} else if p, ok := t.(*ast.PointerType); ok {
		return fmt.Sprintf("ptr_%s", getTypeName(p.ElType))
	} else if b, ok := t.(*ast.BoxType); ok {
		return fmt.Sprintf("box_%s", getTypeName(b.ElType))
	} else if sumType, ok := t.(*ast.SumType); ok {
		cattedOptions := ""
		for i, o := range sumType.Options {
			cattedOptions += getTypeName(o)
			if i != len(sumType.Options)-1 {
				cattedOptions += "_"
			}
		}
		return fmt.Sprintf("sum_%s", cattedOptions)
	}

	panic("Invalid type passed to `getTypeName`.")
}

func getDropFunc(t ast.Type) *ir.Func {
	expectedName := fmt.Sprintf("myc__%s_drop", getTypeName(t))

	for _, f := range module.Funcs {
		if f.Name() == expectedName {
			return f
		}
	}

	panic(fmt.Sprintf("Could not find drop function for type: '%s'.", t.String()))
}

func genMemcpy(src value.Value, dst value.Value, typ ast.Type) {
	if l, ok := src.(*ir.InstLoad); ok {
		src = l.Src
	} else if l, ok := dst.(*ir.InstLoad); ok {
		dst = l.Src
	}

	castedSrc := block.NewBitCast(src, types.I8Ptr)
	castedDst := block.NewBitCast(dst, types.I8Ptr)

	block.NewCall(
		memcpyDeclaration,
		castedDst,
		castedSrc,
		genSizeOf(typ),
		constant.False,
	)
}

func genFunDecl(name string, fun *ast.FunctionType) *ir.Func {
	var foundFunc *ir.Func
	for _, funk := range module.Funcs {
		if funk.Name() == name {
			foundFunc = funk
		}
	}

	if foundFunc != nil {
		return foundFunc
	}

	irParams := []*ir.Param{}

	for _, param := range fun.Parameters {
		irParams = append(irParams, ir.NewParam("", genType(param)))
	}

	var retTyp types.Type
	if name == "main" {
		retTyp = types.I32
	} else if fun.ReturnType == nil {
		retTyp = types.Void
	} else {
		retTyp = genType(fun.ReturnType)
	}

	if _, ok := retTyp.(*types.StructType); ok {
		retParam := ir.NewParam("myc__retval", types.NewPointer(genType(fun.ReturnType)))
		retParam.Attrs = append(retParam.Attrs, ir.SRet{Typ: genType(fun.ReturnType)})

		irParams = append([]*ir.Param{
			retParam,
		}, irParams...)

		retTyp = types.Void
	}

	return module.NewFunc(name, retTyp, irParams...)
}

type llvmStr struct {
	raw string
	def *ir.Global
}

func (l *llvmStr) gep() value.Value {
	return constant.NewGetElementPtr(
		types.NewArray(uint64(len(l.raw)), types.I8),
		l.def,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
}

func createLLVMStr(raw string) *llvmStr {
	l := llvmStr{raw: raw}
	l.def = module.NewGlobalDef("", constant.NewCharArrayFromString(raw))
	l.def.Linkage = enum.LinkagePrivate
	return &l
}

func forwardDeclareModuleExports(importedModule *ast.Module) {
	for _, imp := range importedModule.Imports {
		forwardDeclareModuleExports(imp)
	}

	for _, e := range importedModule.Exports {
		if funDecl, ok := e.(*ast.FunctionType); ok {
			prefixedName := importedModule.Name + "." + funDecl.Name
			namespace[prefixedName] = genFunDecl(prefixedName, funDecl)
		} else if structType, ok := e.(*ast.StructType); ok {
			prefixedName := importedModule.Name + "." + structType.Name

			var fields []types.Type
			for _, m := range structType.Members {
				fields = append(fields, genType(m.Type))
			}

			llvmType := types.NewStruct(fields...)

			var foundTypeDef types.Type
			for _, typeDef := range module.TypeDefs {
				if typeDef.Name() == prefixedName {
					foundTypeDef = typeDef
				}
			}

			if foundTypeDef != nil {
				typespace[prefixedName] = foundTypeDef
			} else {
				typespace[prefixedName] = module.NewTypeDef(prefixedName, llvmType)
			}

			foundDropFunc := false
			dropFuncName := fmt.Sprintf("myc__%s_print", getTypeName(structType))

			for _, funk := range module.Funcs {
				if funk.Name() == dropFuncName {
					foundDropFunc = true
				}
			}

			if !foundDropFunc {
				selfParam := ir.NewParam("", types.NewPointer(genType(structType)))
				decl := module.NewFunc(dropFuncName, types.Void, selfParam)
				decl.Linkage = enum.LinkageExternal
			}
		}
	}
}

func genStatement(stmt ast.Statement, currentModule *ast.Module) {
	switch stmt.(type) {
	case *ast.ImplBlock:
		s := stmt.(*ast.ImplBlock)

		for _, method := range s.Methods {
			method.Parameters = append([]ast.Parameter{
				{
					Identifier: token.Token{
						Lexeme: "self",
						Type:   token.IDENTIFIER,
					},
					Type: &ast.PointerType{ElType: s.Receiver},
				},
			}, method.Parameters...)
			method.Identifier.Lexeme = getTypeName(s.Receiver) + "." + method.Identifier.Lexeme
			genStatement(&method, currentModule)
		}
	case *ast.StructDeclaration:
		s := stmt.(*ast.StructDeclaration)

		var fields []types.Type
		for _, m := range s.Members {
			fields = append(fields, genType(m.Type))
		}

		t := types.NewStruct(fields...)

		var structType types.Type
		if s.Exported && currentModule.Name != "" {
			structType = module.NewTypeDef(currentModule.Name+"."+s.Identifier.Lexeme, t)
		} else {
			structType = module.NewTypeDef(s.Identifier.Lexeme, t)
		}

		typespace[s.Identifier.Lexeme] = structType

		st := ast.StructType{Name: s.Identifier.Lexeme, Members: s.Members, SourceModule: currentModule}
		createStructDrop(&st)
	case *ast.FunctionDeclaration:
		s := stmt.(*ast.FunctionDeclaration)

		funType := s.ToType()

		var fun *ir.Func
		if s.External {
			fun = genFunDecl(s.Identifier.Lexeme, &funType)
			fun.Linkage = enum.LinkageExternal
			namespace[s.Identifier.Lexeme] = fun
			return
		} else if s.Exported && currentModule.Name != "" {
			fun = genFunDecl(currentModule.Name+"."+s.Identifier.Lexeme, &funType)
		} else {
			fun = genFunDecl(s.Identifier.Lexeme, &funType)
			if s.Identifier.Lexeme != "main" {
				fun.Linkage = enum.LinkagePrivate
			}
		}

		currentFunctionsReturnType = s.ReturnType
		funBlock := fun.NewBlock("")

		namespace[s.Identifier.Lexeme] = fun
		shadowed := make(map[string]value.Value)

		for i, param := range s.Parameters {
			var variable *ir.InstAlloca

			if fun.Params[0].Name() == "myc__retval" {
				variable = funBlock.NewAlloca(fun.Params[i+1].Typ)
				funBlock.NewStore(fun.Params[i+1], variable)
			} else {
				variable = funBlock.NewAlloca(fun.Params[i].Typ)
				funBlock.NewStore(fun.Params[i], variable)
			}

			if oldVal, ok := namespace[param.Identifier.Lexeme]; ok {
				shadowed[param.Identifier.Lexeme] = oldVal
			}

			namespace[param.Identifier.Lexeme] = variable
		}

		var varDecls []*ast.VariableDeclaration

		block = funBlock

		currentFunctionsDropBlock = block.Parent.NewBlock("drops")

		for _, stmt := range s.Block.Statements {
			if v, ok := stmt.(*ast.VariableDeclaration); ok {
				varDecls = append(varDecls, v)
			}

			genStatement(stmt, currentModule)
		}

		for _, v := range varDecls {
			if !v.Type.IsCopyable() {
				variable := namespace[v.Identifier.Lexeme]
				currentFunctionsDropBlock.NewCall(getDropFunc(v.Type), variable)
			}
		}

		if s.Identifier.Lexeme == "main" || s.ReturnType == nil {
			if s.Identifier.Lexeme == "main" {
				currentFunctionsDropBlock.NewRet(constant.NewInt(types.I32, 0))
			} else if s.ReturnType == nil {
				currentFunctionsDropBlock.NewRet(nil)
			}
			block.NewBr(currentFunctionsDropBlock)
		} else if currentFunctionsReturnPhi == nil {
			// We have no returns within this function
			//
			// This should not be a problem since actual returns are validated in analysis
			// so we could only reach here if we have changed the functions signature ourselves.
			currentFunctionsDropBlock.NewRet(nil)
		} else {
			retval := currentFunctionsDropBlock.NewPhi(currentFunctionsReturnPhi.Incs...)
			currentFunctionsDropBlock.NewRet(retval)
		}

		block = nil
		currentFunctionsDropBlock = nil
		currentFunctionsReturnType = nil
		currentFunctionsReturnPhi = nil

		for _, param := range s.Parameters {
			delete(namespace, param.Identifier.Lexeme)
			if oldVal, ok := shadowed[param.Identifier.Lexeme]; ok {
				namespace[param.Identifier.Lexeme] = oldVal
			}
		}
	case *ast.VariableDeclaration:
		s := stmt.(*ast.VariableDeclaration)

		variable := block.NewAlloca(genType(s.Type))

		// TODO: Need assure that if s.Value == nil we have to 0-initialize the variable.

		if s.Value != nil {
			variableBoxType, variableIsBox := s.Type.(*ast.BoxType)

			varSumType, variableIsSum := s.Type.(*ast.SumType)
			_, valueIsSum := s.Value.Type().(*ast.SumType)

			if variableIsSum && !valueIsSum {
				genMemcpy(boxExprForSumType(s.Value, varSumType), variable, varSumType)
			} else if variableIsBox && variableBoxType.ElType.Equals(s.Value.Type()) {
				heapPtr := block.NewCall(mallocDeclaration, genSizeOf(s.Value.Type()))
				casted := block.NewBitCast(heapPtr, genType(s.Type))
				genMemcpy(genExpression(s.Value), casted, s.Value.Type())
				block.NewStore(casted, variable)
			} else {
				genMemcpy(genExpression(s.Value), variable, s.Value.Type())
			}
		}

		namespace[s.Identifier.Lexeme] = variable
	case *ast.IfStatement:
		s := stmt.(*ast.IfStatement)

		condition := genExpression(s.Condition)

		iftrueBlock := block.Parent.NewBlock("")
		afterBlock := block.Parent.NewBlock("")
		if s.ElseBlock != nil {
			iffalseBlock := block.Parent.NewBlock("")
			block.NewCondBr(condition, iftrueBlock, iffalseBlock)
			block = iffalseBlock
			genStatement(s.ElseBlock, currentModule)
			if block.Term == nil {
				block.NewBr(afterBlock)
			}
		} else {
			block.NewCondBr(condition, iftrueBlock, afterBlock)
		}

		block = iftrueBlock
		genStatement(&s.IfBlock, currentModule)
		if block.Term == nil {
			block.NewBr(afterBlock)
		}

		block = afterBlock
	case *ast.WhileStatement:
		s := stmt.(*ast.WhileStatement)

		condition := genExpression(s.Condition)
		loopBlock := block.Parent.NewBlock("")
		afterBlock := block.Parent.NewBlock("")
		block.NewCondBr(condition, loopBlock, afterBlock)

		block = loopBlock
		genStatement(&s.Block, currentModule)
		conditionAgain := genExpression(s.Condition)
		block.NewCondBr(conditionAgain, loopBlock, afterBlock)

		block = afterBlock
	case *ast.ReturnStatement:
		s := stmt.(*ast.ReturnStatement)

		if s.Expression == nil {
			block.NewBr(currentFunctionsDropBlock)
		} else {
			retBoxType, shouldReturnBox := currentFunctionsReturnType.(*ast.BoxType)
			retSumType, shouldReturnSum := currentFunctionsReturnType.(*ast.SumType)
			_, exprIsSum := s.Expression.Type().(*ast.SumType)

			var retval value.Value

			if shouldReturnBox && retBoxType.ElType.Equals(s.Expression.Type()) {
				heapPtr := block.NewCall(mallocDeclaration, genSizeOf(s.Expression.Type()))
				casted := block.NewBitCast(heapPtr, block.Parent.Sig.RetType)
				block.NewStore(genExpression(s.Expression), casted)
				retval = casted
			} else if shouldReturnSum && !exprIsSum {
				retval = boxExprForSumType(s.Expression, retSumType)
			} else {
				retval = genExpression(s.Expression)
			}

			if len(block.Parent.Sig.Params) > 0 && block.Parent.Params[0].Name() == "myc__retval" {
				// If returning a struct, we copy the return value into our `sret` param
				genMemcpy(retval, block.Parent.Params[0], s.Expression.Type())
				block.NewBr(currentFunctionsDropBlock)
			} else {
				preReturnBlock := block.Parent.NewBlock("")
				block.NewBr(preReturnBlock)
				preReturnBlock.NewBr(currentFunctionsDropBlock)

				inc := ir.NewIncoming(retval, preReturnBlock)
				if currentFunctionsReturnPhi == nil {
					currentFunctionsReturnPhi = ir.NewPhi(inc)
					currentFunctionsReturnPhi.Typ = genType(currentFunctionsReturnType)
				} else {
					currentFunctionsReturnPhi.Incs = append(currentFunctionsReturnPhi.Incs, inc)
				}
			}
		}
	case *ast.ImportStatement:
		// We forward declare all of the imported modules declarations
		s := stmt.(*ast.ImportStatement)
		importedModule := currentModule.Imports[s.Identifier.Lexeme]
		forwardDeclareModuleExports(importedModule)
	case *ast.ExpressionStatement:
		s := stmt.(*ast.ExpressionStatement)
		genExpression(s.Expression)
	case *ast.BlockStatement:
		s := stmt.(*ast.BlockStatement)
		for _, stmt := range s.Statements {
			genStatement(stmt, currentModule)
		}
	}
}

func boxExprForSumType(v ast.Expression, sumType *ast.SumType) value.Value {
	sumTypeStructType := genType(sumType)
	tempSumBox := block.NewAlloca(sumTypeStructType)

	typIdxPtr := block.NewGetElementPtr(
		sumTypeStructType,
		tempSumBox,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	typIdxIntType := typIdxPtr.Type().(*types.PointerType).ElemType.(*types.IntType)

	casted := block.NewBitCast(
		tempSumBox,
		types.NewPointer(
			types.NewStruct(typIdxIntType, genType(v.Type())),
		),
	)

	dataPtr := block.NewGetElementPtr(
		types.NewStruct(typIdxIntType, genType(v.Type())),
		casted,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)

	foundIdx := -1
	for i, o := range sumType.Options {
		if o.Equals(v.Type()) {
			foundIdx = i
		}
	}

	block.NewStore(
		constant.NewInt(typIdxIntType, int64(foundIdx)),
		typIdxPtr,
	)

	block.NewStore(genExpression(v), dataPtr)
	return block.NewLoad(sumTypeStructType, tempSumBox)
}

func genExpression(expr ast.Expression) value.Value {
	switch expr.(type) {
	case *ast.UnaryExpression:
		e := expr.(*ast.UnaryExpression)
		switch e.Operator.Type {
		case token.MINUS:
			return block.NewSub(constant.NewInt(types.I32, 0), genExpression(e.Value))
		}
	case *ast.BinaryExpression:
		e := expr.(*ast.BinaryExpression)

		if e.Operator.Type == token.EQUAL {
			targetBoxType, targetIsBox := e.Left.Type().(*ast.BoxType)

			targetSumType, targetIsSum := e.Left.Type().(*ast.SumType)
			_, valueIsSum := e.Right.Type().(*ast.SumType)

			var target value.Value

			if deref, ok := e.Left.(*ast.Dereference); ok {
				// If assigning to a dereference we generate it's nested expression instead.
				target = genExpression(deref.Expression)
			} else {
				expr := genExpression(e.Left)
				if load, ok := expr.(*ir.InstLoad); ok {
					target = load.Src
				} else {
					target = expr
				}
			}

			if targetIsSum && !valueIsSum {
				genMemcpy(boxExprForSumType(e.Right, targetSumType), target, targetSumType)
			} else if targetIsBox && targetBoxType.ElType.Equals(e.Right.Type()) {
				// We dereference the heap pointer and copy the value instead
				loadedTarget := block.NewLoad(genType(e.Left.Type()), target)
				genMemcpy(genExpression(e.Right), loadedTarget, e.Right.Type())
			} else if !e.Left.Type().IsCopyable() {
				// No-copy types can only have rvalues aassigned to them
				// We can assume that the rhs will create a new heap-based value
				// which we can copy into the lhs. This means we can free the lhs'
				// value.
				block.NewCall(getDropFunc(e.Left.Type()), target)
				genMemcpy(genExpression(e.Right), target, e.Right.Type())
			} else {
				genMemcpy(genExpression(e.Right), target, e.Right.Type())
			}

			return block.NewLoad(genType(e.Left.Type()), target)
		}

		var result *ir.InstAlloca
		if e.Operator.Type.IsComparativeOperator() {
			result = block.NewAlloca(types.I1)
		} else {
			result = block.NewAlloca(genType(e.Left.Type()))
		}

		switch e.Operator.Type {
		case token.PLUS:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFAdd(
						genExpression(e.Left), genExpression(e.Right),
					),
					result,
				)
				return block.NewLoad(genType(e.Left.Type()), result)
			} else if e.Left.Type().(*ast.Primitive).Kind == ast.Str {
				left := genExpression(e.Left)
				right := genExpression(e.Right)

				if l, ok := left.(*ir.InstLoad); ok {
					left = l.Src
				}

				if l, ok := right.(*ir.InstLoad); ok {
					right = l.Src
				}

				leftBufferPtr := block.NewGetElementPtr(
					genType(e.Type()), left,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				leftBuffer := block.NewLoad(types.I8Ptr, leftBufferPtr)

				rightBufferPtr := block.NewGetElementPtr(
					genType(e.Type()), right,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				rightBuffer := block.NewLoad(types.I8Ptr, rightBufferPtr)

				leftLengthPtr := block.NewGetElementPtr(
					genType(e.Type()), left,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 1),
				)
				rightLengthPtr := block.NewGetElementPtr(
					genType(e.Type()), right,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 1),
				)
				leftLength := block.NewLoad(types.I64, leftLengthPtr)
				rightLength := block.NewLoad(types.I64, rightLengthPtr)

				newBufferPtr := block.NewGetElementPtr(
					genType(e.Type()), result,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				)
				newLength := block.NewGetElementPtr(
					genType(e.Type()), result,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 1),
				)
				newIsHeap := block.NewGetElementPtr(
					genType(e.Type()), result,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 2),
				)

				totalLength := block.NewAdd(leftLength, rightLength)
				block.NewStore(totalLength, newLength)

				block.NewStore(block.NewCall(mallocDeclaration, totalLength), newBufferPtr)
				block.NewStore(constant.True, newIsHeap)

				newBuffer := block.NewLoad(types.I8Ptr, newBufferPtr)
				newBufferOffsetByLeft := block.NewGetElementPtr(types.I8, newBuffer, leftLength)

				block.NewCall(memcpyDeclaration, newBuffer, leftBuffer, leftLength, constant.NewInt(types.I1, 0))
				block.NewCall(memcpyDeclaration, newBufferOffsetByLeft, rightBuffer, rightLength, constant.NewInt(types.I1, 0))
			} else {
				block.NewStore(
					block.NewAdd(
						genExpression(e.Left), genExpression(e.Right),
					),
					result,
				)
			}
		case token.MINUS:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFSub(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewSub(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.STAR:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFMul(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewMul(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.SLASH:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFDiv(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else if e.Left.Type().(*ast.Primitive).Kind.IsUnsignedInteger() {
				block.NewStore(
					block.NewUDiv(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewSDiv(genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.LESSER:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredOLT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else if e.Left.Type().(*ast.Primitive).Kind.IsUnsignedInteger() {
				block.NewStore(
					block.NewICmp(enum.IPredULT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewICmp(enum.IPredSLT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.GREATER:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredOGT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else if e.Left.Type().(*ast.Primitive).Kind.IsUnsignedInteger() {
				block.NewStore(
					block.NewICmp(enum.IPredUGT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewICmp(enum.IPredSGT, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.LESSER_EQUAL:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredOLE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else if e.Left.Type().(*ast.Primitive).Kind.IsUnsignedInteger() {
				block.NewStore(
					block.NewICmp(enum.IPredULE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewICmp(enum.IPredSLE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.GREATER_EQUAL:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredOGE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else if e.Left.Type().(*ast.Primitive).Kind.IsUnsignedInteger() {
				block.NewStore(
					block.NewICmp(enum.IPredUGE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			} else {
				block.NewStore(
					block.NewICmp(enum.IPredSGE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
		case token.EQUAL_EQUAL:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredOEQ, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
			// ICmp works for both `i*`, `u*`, and `bool`
			block.NewStore(
				block.NewICmp(enum.IPredEQ, genExpression(e.Left), genExpression(e.Right)),
				result,
			)
		case token.BANG_EQUAL:
			if e.Left.Type().(*ast.Primitive).Kind.IsFloatingPoint() {
				block.NewStore(
					block.NewFCmp(enum.FPredONE, genExpression(e.Left), genExpression(e.Right)),
					result,
				)
			}
			block.NewStore(
				block.NewICmp(enum.IPredNE, genExpression(e.Left), genExpression(e.Right)),
				result,
			)
		case token.AND_AND:
			oldBlock := block

			left := genExpression(e.Left)
			ifLeftTrueBlock := block.Parent.NewBlock("")
			otherwiseBlock := block.Parent.NewBlock("")

			block.NewCondBr(left, ifLeftTrueBlock, otherwiseBlock)

			block = ifLeftTrueBlock
			right := genExpression(e.Right)
			ifLeftTrueBlock.NewBr(otherwiseBlock)

			block = otherwiseBlock
			block.NewStore(
				block.NewPhi(
					ir.NewIncoming(constant.False, oldBlock),
					ir.NewIncoming(right, ifLeftTrueBlock),
				),
				result,
			)
		case token.OR_OR:
			left := genExpression(e.Left)
			right := genExpression(e.Right)
			block.NewStore(block.NewOr(left, right), result)
		}

		return block.NewLoad(genType(e.Type()), result)
	case *ast.CallExpression:
		e := expr.(*ast.CallExpression)
		args := []value.Value{}
		for i, arg := range e.Arguments {
			paramBoxType, paramIsBox := e.Callee.Type().(*ast.FunctionType).Parameters[i].(*ast.BoxType)

			paramSumType, paramIsSum := e.Callee.Type().(*ast.FunctionType).Parameters[i].(*ast.SumType)
			_, argIsSum := arg.Type().(*ast.SumType)

			// TODO: also need to handle a no-copy rvalue
			if paramIsBox && paramBoxType.ElType.Equals(arg.Type()) {
				// Assigning an unboxed value to a box. Example:
				//
				//     fun printBox(b ~int) {
				//         print b
				//     }
				//     printBox(56) // we are genning this
				//
				tempBox := block.NewCall(mallocDeclaration, genSizeOf(arg.Type()))
				args = append(args, block.NewBitCast(tempBox, genType(paramBoxType)))
				currentFunctionsDropBlock.NewCall(getDropFunc(paramBoxType), tempBox)
			} else if paramIsSum && !argIsSum {
				args = append(args, boxExprForSumType(arg, paramSumType))
			} else {
				args = append(args, genExpression(arg))
			}
		}

		var callee value.Value
		if varExpr, ok := e.Callee.(*ast.VariableExpression); ok {
			callee = namespace[varExpr.Identifier.Lexeme]
		} else if getExpr, ok := e.Callee.(*ast.GetExpression); ok {
			callee = genExpression(getExpr)
			args = append([]value.Value{genExpression(getExpr.Expression).(*ir.InstLoad).Src}, args...)
		} else {
			panic("Can only generate variables or get expressions as calle.")
		}

		if callee.(*ir.Func).Sig.RetType != types.Void {
			called := block.NewCall(callee, args...)
			result := block.NewAlloca(callee.(*ir.Func).Sig.RetType)
			block.NewStore(called, result)
			return block.NewLoad(callee.(*ir.Func).Sig.RetType, result)
		} else if len(callee.(*ir.Func).Params) > 0 && callee.(*ir.Func).Params[0].Name() == "myc__retval" {
			retTyp := callee.(*ir.Func).Params[0].Type().(*types.PointerType).ElemType
			result := block.NewAlloca(retTyp)
			args = append([]value.Value{result}, args...)
			block.NewCall(callee, args...)
			return block.NewLoad(retTyp, result)
		} else {
			return block.NewCall(callee, args...)
		}
	case *ast.VariableExpression:
		e := expr.(*ast.VariableExpression)
		resolvedVariable := namespace[e.Identifier.Lexeme]
		return block.NewLoad(resolvedVariable.Type().(*types.PointerType).ElemType, resolvedVariable)
	case *ast.GetExpression:
		e := expr.(*ast.GetExpression)

		structType, isStruct := e.Expression.Type().(*ast.StructType)
		_, isPrimitive := e.Expression.Type().(*ast.Primitive)
		_, isSlice := e.Expression.Type().(*ast.SliceType)

		if isStruct {
			if _, ok := structType.MethodTable[e.Identifier.Lexeme]; ok {
				var foundFunc *ir.Func
				for _, fun := range module.Funcs {
					if fun.Name() == getTypeName(structType)+"."+e.Identifier.Lexeme {
						foundFunc = fun
					}
				}

				if foundFunc == nil {
					panic("internal error: could not resolve method name on struct type")
				}

				return foundFunc
			} else {
				memberIndex := 0
				for i, m := range structType.Members {
					if m.Identifier.Lexeme == e.Identifier.Lexeme {
						memberIndex = i
					}
				}

				expr := genExpression(e.Expression)
				var gep *ir.InstGetElementPtr

				if loadInst, ok := expr.(*ir.InstLoad); ok {
					gep = block.NewGetElementPtr(
						genType(structType),
						loadInst.Src,
						constant.NewInt(types.I32, int64(0)),
						constant.NewInt(types.I32, int64(memberIndex)),
					)
				} else {
					panic("internal error: get expression on non-load instruction.")
				}

				return block.NewLoad(genType(structType.Members[memberIndex].Type), gep)
			}
		} else if isPrimitive || isSlice {
			var foundFunc *ir.Func
			funName := getTypeName(e.Expression.Type()) + "." + e.Identifier.Lexeme

			for _, fun := range module.Funcs {
				if fun.Name() == funName {
					foundFunc = fun
				}
			}

			if foundFunc == nil {
				panic("internal error: could not resolve method name on primitive")
			}

			return foundFunc
		} else if module, ok := e.Expression.Type().(*ast.Module); ok {
			return namespace[module.Name+"."+e.Identifier.Lexeme]
		}
	case *ast.IndexExpression:
		e := expr.(*ast.IndexExpression)
		target := genExpression(e.Expression)
		index := genExpression(e.Index)

		if index.Type().(*types.IntType).BitSize == 32 {
			index = block.NewZExt(index, types.I64)
		}

		eltype := e.Expression.Type().(*ast.SliceType).ElType

		if loadInst, ok := target.(*ir.InstLoad); ok {
			t0 := block.NewGetElementPtr(
				genType(e.Expression.Type()),
				loadInst.Src,
				constant.NewInt(types.I32, int64(0)),
				constant.NewInt(types.I32, int64(0)),
			)

			t1 := block.NewGetElementPtr(
				genType(e.Expression.Type()),
				loadInst.Src,
				constant.NewInt(types.I32, int64(0)),
				constant.NewInt(types.I32, int64(1)),
			)

			buffer := block.NewLoad(types.NewPointer(genType(eltype)), t0)
			length := block.NewLoad(types.I64, t1)

			idxOutOfBoundsBlock := block.Parent.NewBlock("")
			afterBlock := block.Parent.NewBlock("")

			idxLessThan0 := block.NewICmp(enum.IPredSLT, index, constant.NewInt(types.I64, 0))
			idxGreaterThanLen := block.NewICmp(enum.IPredSGT, index, length)
			idxNotOk := block.NewOr(idxGreaterThanLen, idxLessThan0)

			block.NewCondBr(idxNotOk, idxOutOfBoundsBlock, afterBlock)

			message := createLLVMStr("runtime-error: index out of bounds\x0A\x00")
			idxOutOfBoundsBlock.NewCall(panicDeclaration, message.gep())
			idxOutOfBoundsBlock.NewUnreachable()

			block = afterBlock

			ptrToValue := block.NewGetElementPtr(
				genType(eltype),
				buffer,
				index,
			)

			return block.NewLoad(genType(eltype), ptrToValue)
		} else {
			panic("internal error: index expression on non-load instruction.")
		}
	case *ast.IsExpression:
		e := expr.(*ast.IsExpression)

		idxOfComparedType := -1
		for i, o := range e.Expression.Type().(*ast.SumType).Options {
			if o.Equals(e.ComparedType) {
				idxOfComparedType = i
			}
		}

		left := genExpression(e.Expression)

		if load, ok := left.(*ir.InstLoad); ok {
			left = load.Src
		}

		leftIdx := block.NewGetElementPtr(genType(e.Expression.Type()), left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
		idxIntType := leftIdx.Type().(*types.PointerType).ElemType.(*types.IntType)
		loadedIdx := block.NewLoad(idxIntType, leftIdx)

		comparison := block.NewICmp(enum.IPredEQ, loadedIdx, constant.NewInt(idxIntType, int64(idxOfComparedType)))
		return comparison
	case *ast.AsExpression:
		e := expr.(*ast.AsExpression)

		if _, ok := e.Expression.Type().(*ast.SumType); ok {
			idxOfTargetType := -1
			for i, o := range e.Expression.Type().(*ast.SumType).Options {
				if o.Equals(e.TargetType) {
					idxOfTargetType = i
				}
			}

			left := genExpression(e.Expression)

			if load, ok := left.(*ir.InstLoad); ok {
				left = load.Src
			}

			leftIdx := block.NewGetElementPtr(genType(e.Expression.Type()), left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 0))
			idxIntType := leftIdx.Type().(*types.PointerType).ElemType.(*types.IntType)
			loadedIdx := block.NewLoad(idxIntType, leftIdx)

			comparison := block.NewICmp(enum.IPredEQ, loadedIdx, constant.NewInt(idxIntType, int64(idxOfTargetType)))

			castOkBlock := block.Parent.NewBlock("")
			castFailedBlock := block.Parent.NewBlock("")
			block.NewCondBr(comparison, castOkBlock, castFailedBlock)

			errMessage := createLLVMStr("runtime-error: sum-type cast failed\x0A\x00")
			castFailedBlock.NewCall(panicDeclaration, errMessage.gep())
			castFailedBlock.NewUnreachable()

			block = castOkBlock

			castedVariable := block.NewAlloca(genType(e.TargetType))
			dataPtr := block.NewGetElementPtr(genType(e.Expression.Type()), left, constant.NewInt(types.I32, 0), constant.NewInt(types.I32, 1))
			loadedPtr := block.NewLoad(types.I8Ptr, dataPtr)
			value := block.NewLoad(genType(e.TargetType), block.NewBitCast(loadedPtr, types.NewPointer(genType(e.TargetType))))
			block.NewStore(value, castedVariable)

			return block.NewLoad(genType(e.TargetType), castedVariable)
		} else if exprPrimType, ok := e.Expression.Type().(*ast.Primitive); ok {
			result := block.NewAlloca(genType(e.TargetType))

			targetPrimType, ok := e.TargetType.(*ast.Primitive)

			if !ok {
				panic("internal error: Can only cast from primitive type to primitive type.")
			}

			if exprPrimType.Kind.IsFloatingPoint() && targetPrimType.Kind.IsFloatingPoint() {
				if exprPrimType.Kind > targetPrimType.Kind {
					block.NewStore(block.NewFPExt(genExpression(e.Expression), genType(e.TargetType)), result)
				} else {
					block.NewStore(block.NewFPTrunc(genExpression(e.Expression), genType(e.TargetType)), result)
				}
			} else {
				exprSize := getPackedSize(exprPrimType)
				targetSize := getPackedSize(targetPrimType)

				if exprSize == targetSize {
					block.NewStore(genExpression(e.Expression), result)
				} else if exprSize > targetSize {
					block.NewStore(block.NewTrunc(genExpression(e.Expression), genType(e.TargetType)), result)
				} else {
					block.NewStore(block.NewSExt(genExpression(e.Expression), genType(e.TargetType)), result)
				}
			}

			return block.NewLoad(genType(e.TargetType), result)
		}

		panic("internal error: Unimplemented as expression combination.")
	case *ast.CompositeLiteral:
		e := expr.(*ast.CompositeLiteral)
		st := e.Typ.(*ast.StructType)
		t := genType(st)

		local := block.NewAlloca(t)

		var initializers []ast.Expression
		initializers = make([]ast.Expression, len(st.Members))

		if e.NamedInitializers != nil {
			for _, init := range *e.NamedInitializers {
				memberIndex := -1
				for i, m := range st.Members {
					if m.Identifier.Lexeme == init.Identifier.Lexeme {
						memberIndex = i
					}
				}

				initializers[memberIndex] = init.Value
			}
		} else {
			initializers = *e.UnnamedInitializers
		}

		for i, init := range initializers {
			tmp := block.NewGetElementPtr(
				t,
				local,
				constant.NewInt(types.I32, int64(0)),
				constant.NewInt(types.I32, int64(i)),
			)

			boxType, memberIsBox := st.Members[i].Type.(*ast.BoxType)

			sumType, memberIsSum := st.Members[i].Type.(*ast.SumType)
			_, initIsSum := init.Type().(*ast.SumType)

			if memberIsBox && boxType.ElType.Equals(init.Type()) {
				box := block.NewCall(mallocDeclaration, genSizeOf(boxType.ElType))
				casted := block.NewBitCast(box, genType(boxType))
				genMemcpy(genExpression(init), casted, init.Type())
				block.NewStore(casted, tmp)
			} else if memberIsSum && !initIsSum {
				genMemcpy(boxExprForSumType(init, sumType), tmp, sumType)
			} else {
				genMemcpy(genExpression(init), tmp, init.Type())
			}
		}

		return block.NewLoad(t, local)
	case *ast.SliceLiteral:
		e := expr.(*ast.SliceLiteral)
		local := block.NewAlloca(genType(e.Type()))
		eltype := e.Type().(*ast.SliceType).ElType
		elBoxType, elTypeIsBox := eltype.(*ast.BoxType)
		elSumType, elTypeIsSum := eltype.(*ast.SumType)

		buffer := block.NewGetElementPtr(
			genType(e.Type()),
			local,
			constant.NewInt(types.I32, int64(0)),
			constant.NewInt(types.I32, int64(0)),
		)

		length := block.NewGetElementPtr(
			genType(e.Type()),
			local,
			constant.NewInt(types.I32, int64(0)),
			constant.NewInt(types.I32, int64(1)),
		)

		capacity := block.NewGetElementPtr(
			genType(e.Type()),
			local,
			constant.NewInt(types.I32, int64(0)),
			constant.NewInt(types.I32, int64(2)),
		)

		block.NewStore(constant.NewInt(types.I64, int64(len(e.Expressions))), length)
		if len(e.Expressions) == 0 {
			block.NewStore(constant.NewInt(types.I64, 8), capacity)
		} else {
			block.NewStore(constant.NewInt(types.I64, int64(len(e.Expressions))), capacity)
		}

		ptrmem := block.NewCall(mallocDeclaration, block.NewMul(
			genSizeOf(eltype), block.NewLoad(types.I64, capacity),
		))
		casted := block.NewBitCast(ptrmem, types.NewPointer(genType(eltype)))
		block.NewStore(casted, buffer)

		for i, value := range e.Expressions {
			_, valueIsSum := value.Type().(*ast.SumType)

			loadedBuffer := block.NewLoad(types.NewPointer(genType(eltype)), buffer)
			pointerToElement := block.NewGetElementPtr(genType(eltype), loadedBuffer, constant.NewInt(types.I64, int64(i)))

			if elTypeIsBox && elBoxType.ElType.Equals(value.Type()) {
				temporaryBox := block.NewCall(mallocDeclaration, genSizeOf(value.Type()))
				block.NewStore(block.NewBitCast(temporaryBox, genType(elBoxType)), pointerToElement)
			} else if elTypeIsSum && !valueIsSum {
				genMemcpy(boxExprForSumType(value, elSumType), pointerToElement, elSumType)
			} else {
				genMemcpy(genExpression(value), pointerToElement, value.Type())
			}
		}

		return block.NewLoad(genType(e.Type()), local)
	case *ast.ReferenceOf:
		e := expr.(*ast.ReferenceOf)
		target := genExpression(e.Target)

		if loadInst, ok := target.(*ir.InstLoad); ok {
			return loadInst.Src
		} else {
			panic("Internal error: only load instructions can have their reference taken.")
		}
	case *ast.Dereference:
		e := expr.(*ast.Dereference)
		return block.NewLoad(genType(e.Type()), genExpression(e.Expression))
	case *ast.Literal:
		e := expr.(*ast.Literal)
		switch e.Token.Type {
		case token.STRING:
			staticBuffer := createLLVMStr(e.LiteralValue)
			str := block.NewAlloca(genType(e.Type()))
			buffer := block.NewGetElementPtr(
				genType(e.Type()), str,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			)
			length := block.NewGetElementPtr(
				genType(e.Type()), str,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			isHeap := block.NewGetElementPtr(
				genType(e.Type()), str,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 2),
			)
			block.NewStore(constant.False, isHeap)
			block.NewStore(constant.NewInt(types.I64, int64(len(e.LiteralValue))), length)
			block.NewStore(staticBuffer.gep(), buffer)
			return block.NewLoad(genType(e.Type()), str)
		case token.INT:
			parsedInt, _ := strconv.ParseInt(e.LiteralValue, 10, 32)
			tmp := block.NewAlloca(types.I32)
			block.NewStore(constant.NewInt(types.I32, parsedInt), tmp)
			return block.NewLoad(types.I32, tmp)
		case token.FLOAT:
			parsedFloat, _ := strconv.ParseFloat(e.LiteralValue, 64)
			tmp := block.NewAlloca(types.Double)
			block.NewStore(constant.NewFloat(types.Double, parsedFloat), tmp)
			return block.NewLoad(types.Double, tmp)
		case token.TRUE:
			tmp := block.NewAlloca(types.I1)
			block.NewStore(constant.NewInt(types.I1, 1), tmp)
			return block.NewLoad(types.I1, tmp)
		case token.FALSE:
			tmp := block.NewAlloca(types.I1)
			block.NewStore(constant.NewInt(types.I1, 0), tmp)
			return block.NewLoad(types.I1, tmp)
		}
	}

	panic(fmt.Sprintf("Expression node has invalid static type. %T", expr))
}

func createStructDrop(structType *ast.StructType) {
	funcName := fmt.Sprintf("myc__%s_drop", getTypeName(structType))

	for _, funk := range module.Funcs {
		if funk.Name() == funcName {
			return
		}
	}

	selfParam := ir.NewParam("", types.NewPointer(genType(structType)))
	structDrop := module.NewFunc(funcName, types.Void, selfParam)
	structDropBlock := structDrop.NewBlock("")

	for i, m := range structType.Members {
		if !m.Type.IsCopyable() {
			var memberDropFunc *ir.Func
			memberDropFuncName := fmt.Sprintf("myc__%s_drop", getTypeName(m.Type))

			for _, f := range module.Funcs {
				if f.Name() == memberDropFuncName {
					memberDropFunc = f
				}
			}

			if memberDropFunc == nil {
				panic("Internal error: could not find drop function for struct member.")
			}

			memberPtr := structDropBlock.NewGetElementPtr(
				genType(structType),
				selfParam,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(i)),
			)

			structDropBlock.NewCall(memberDropFunc, memberPtr)
		}
	}

	structDropBlock.NewRet(nil)
}

func ensureSliceMethods(sliceType *ast.SliceType, llvmType types.Type) {
	sliceDropExists := false
	sliceDropFuncName := fmt.Sprintf("myc__slice_%s_drop", getTypeName(sliceType.ElType))

	for _, f := range module.Funcs {
		if f.Name() == sliceDropFuncName {
			sliceDropExists = true
		}
	}

	if sliceDropExists {
		return
	}

	selfParam := ir.NewParam("", types.NewPointer(llvmType))

	// slice.drop()
	sliceDrop := module.NewFunc(sliceDropFuncName, types.Void, selfParam)
	sliceDrop.Linkage = enum.LinkagePrivate
	sliceDropBlock := sliceDrop.NewBlock("")

	buffer := sliceDropBlock.NewGetElementPtr(
		llvmType,
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	lenPtr := sliceDropBlock.NewGetElementPtr(
		llvmType,
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	loadedBuffer := sliceDropBlock.NewLoad(types.NewPointer(genType(sliceType.ElType)), buffer)
	length := sliceDropBlock.NewLoad(types.I64, lenPtr)

	loopBlock := sliceDrop.NewBlock("")
	afterBlock := sliceDrop.NewBlock("")

	idxVar := sliceDropBlock.NewAlloca(types.I64)
	sliceDropBlock.NewStore(constant.NewInt(types.I64, 0), idxVar)
	sliceDropBlock.NewBr(loopBlock)

	if !sliceType.ElType.IsCopyable() {
		loopBlock.NewCall(getDropFunc(sliceType.ElType), loopBlock.NewGetElementPtr(
			genType(sliceType.ElType),
			loadedBuffer,
			loopBlock.NewLoad(types.I64, idxVar),
		))
	}

	loopBlock.NewStore(loopBlock.NewAdd(loopBlock.NewLoad(types.I64, idxVar), constant.NewInt(types.I64, 1)), idxVar)
	idxLessThanLen := loopBlock.NewICmp(enum.IPredULT, loopBlock.NewLoad(types.I64, idxVar), length)
	loopBlock.NewCondBr(idxLessThanLen, loopBlock, afterBlock)

	casted := afterBlock.NewBitCast(afterBlock.NewLoad(
		types.NewPointer(genType(sliceType.ElType)), buffer,
	), types.I8Ptr)

	afterBlock.NewCall(freeDeclaration, casted)
	afterBlock.NewStore(constant.NewNull(types.NewPointer(genType(sliceType.ElType))), buffer)
	afterBlock.NewRet(nil)

	// slice.len()
	sliceLen := module.NewFunc(fmt.Sprintf("slice_%s.len", getTypeName(sliceType.ElType)), types.I64, selfParam)
	sliceLen.Linkage = enum.LinkagePrivate
	sliceLenBlock := sliceLen.NewBlock("")
	lenPtr = sliceLenBlock.NewGetElementPtr(
		llvmType,
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	sliceLenBlock.NewRet(sliceLenBlock.NewLoad(types.I64, lenPtr))

	// slice.buf()
	sliceBuf := module.NewFunc(
		fmt.Sprintf("slice_%s.buf", getTypeName(sliceType.ElType)),
		types.NewPointer(genType(sliceType.ElType)), selfParam,
	)
	sliceBuf.Linkage = enum.LinkagePrivate
	sliceBufBlock := sliceBuf.NewBlock("")

	bufPtr := sliceBufBlock.NewGetElementPtr(
		llvmType,
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	sliceBufBlock.NewRet(sliceBufBlock.NewLoad(
		types.NewPointer(genType(sliceType.ElType)),
		bufPtr,
	))
}

func ensureBoxDrop(boxType *ast.BoxType, llvmType types.Type) {
	boxDropFuncName := fmt.Sprintf("myc__box_%s_drop", getTypeName(boxType.ElType))

	for _, f := range module.Funcs {
		if f.Name() == boxDropFuncName {
			return
		}
	}

	selfParam := ir.NewParam("", types.NewPointer(llvmType))
	boxDrop := module.NewFunc(boxDropFuncName, types.Void, selfParam)
	boxDropBlock := boxDrop.NewBlock("")
	casted := boxDropBlock.NewBitCast(boxDropBlock.NewLoad(llvmType, selfParam), types.I8Ptr)
	boxDropBlock.NewCall(freeDeclaration, casted)
	boxDropBlock.NewStore(constant.NewNull(llvmType.(*types.PointerType)), selfParam)
	boxDropBlock.NewRet(nil)
}

func ensureSumTypeDrop(sumType *ast.SumType, llvmType types.Type) {
	cattedOptions := ""
	for i, o := range sumType.Options {
		cattedOptions += getTypeName(o)
		if i != len(sumType.Options)-1 {
			cattedOptions += "_"
		}
	}
	sumDropFuncName := fmt.Sprintf("myc__sum_%s_drop", cattedOptions)

	for _, f := range module.Funcs {
		if f.Name() == sumDropFuncName {
			return
		}
	}

	selfParam := ir.NewParam("", types.NewPointer(llvmType))

	sumDrop := module.NewFunc(sumDropFuncName, types.Void, selfParam)
	sumDropBlock := sumDrop.NewBlock("")

	typIdxPtr := sumDropBlock.NewGetElementPtr(
		llvmType,
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	typIdxIntType := typIdxPtr.Type().(*types.PointerType).ElemType.(*types.IntType)

	invalidTypIdxBlock := sumDrop.NewBlock("")
	var cases []*ir.Case
	for i, o := range sumType.Options {
		caseBlock := sumDrop.NewBlock("")
		if !o.IsCopyable() {
			data := caseBlock.NewGetElementPtr(
				types.NewStruct(typIdxIntType, genType(o)),
				selfParam,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 1),
			)
			caseBlock.NewCall(getDropFunc(o), data)
		}
		caseBlock.NewRet(nil)
		cases = append(cases, ir.NewCase(constant.NewInt(typIdxIntType, int64(i)), caseBlock))
	}
	sumDropBlock.NewSwitch(sumDropBlock.NewLoad(typIdxIntType, typIdxPtr), invalidTypIdxBlock, cases...)

	errMessage := createLLVMStr("runtime-error: sum-type has invalid index\x0A\x00")
	invalidTypIdxBlock.NewCall(panicDeclaration, errMessage.gep())
	invalidTypIdxBlock.NewUnreachable()
}

func declarePrimitiveMethods() {
	selfParam := ir.NewParam("", types.NewPointer(genType(&ast.Primitive{Kind: ast.Str})))

	// str.drop()
	module.NewFunc("myc__str_drop", types.Void, selfParam)

	byteSliceType := genType(&ast.SliceType{ElType: &ast.Primitive{Kind: ast.U8}})
	retParam := ir.NewParam("myc__retval", types.NewPointer(byteSliceType))
	retParam.Attrs = append(retParam.Attrs, ir.SRet{Typ: types.NewPointer(byteSliceType)})
	module.NewFunc(
		"str.bytes",
		types.Void,
		retParam,
		selfParam,
	)
}

func createPrimitiveMethods() {
	selfParam := ir.NewParam("", types.NewPointer(genType(&ast.Primitive{Kind: ast.Str})))

	// str.drop()
	strDrop := module.NewFunc("myc__str_drop", types.Void, selfParam)

	checkHeapBlock := strDrop.NewBlock("")
	strDropBlock := strDrop.NewBlock("")
	endBlock := strDrop.NewBlock("")

	isHeapPtr := checkHeapBlock.NewGetElementPtr(
		genType(&ast.Primitive{Kind: ast.Str}),
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 2),
	)
	isHeap := checkHeapBlock.NewLoad(types.I1, isHeapPtr)
	check := checkHeapBlock.NewICmp(enum.IPredEQ, isHeap, constant.True)
	checkHeapBlock.NewCondBr(check, strDropBlock, endBlock)

	buffer := strDropBlock.NewGetElementPtr(
		genType(&ast.Primitive{Kind: ast.Str}),
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)

	strDropBlock.NewCall(freeDeclaration, strDropBlock.NewLoad(types.I8Ptr, buffer))
	strDropBlock.NewStore(constant.NewNull(types.I8Ptr), buffer)
	strDropBlock.NewBr(endBlock)

	endBlock.NewRet(nil)

	// str.bytes()
	byteSliceType := genType(&ast.SliceType{ElType: &ast.Primitive{Kind: ast.U8}})
	retParam := ir.NewParam("myc__retval", types.NewPointer(byteSliceType))
	retParam.Attrs = append(retParam.Attrs, ir.SRet{Typ: types.NewPointer(byteSliceType)})

	strBytes := module.NewFunc(
		"str.bytes",
		types.Void,
		retParam,
		selfParam,
	)

	strBytesBlock := strBytes.NewBlock("")
	result := strBytesBlock.NewAlloca(byteSliceType)

	bufPtr := strBytesBlock.NewGetElementPtr(
		genType(&ast.Primitive{Kind: ast.Str}),
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	lenPtr := strBytesBlock.NewGetElementPtr(
		genType(&ast.Primitive{Kind: ast.Str}),
		selfParam,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)

	resultBufPtr := strBytesBlock.NewGetElementPtr(
		byteSliceType,
		result,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	resultLenPtr := strBytesBlock.NewGetElementPtr(
		byteSliceType,
		result,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	resultCapPtr := strBytesBlock.NewGetElementPtr(
		byteSliceType,
		result,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 2),
	)

	len := strBytesBlock.NewLoad(types.I64, lenPtr)
	strBytesBlock.NewStore(strBytesBlock.NewLoad(types.NewPointer(types.I8), bufPtr), resultBufPtr)
	heapMemory := strBytesBlock.NewCall(mallocDeclaration, len)

	strBytesBlock.NewCall(
		memcpyDeclaration,
		heapMemory,
		strBytesBlock.NewLoad(types.NewPointer(types.I8), bufPtr),
		len,
		constant.False,
	)

	strBytesBlock.NewStore(heapMemory, resultBufPtr)

	strBytesBlock.NewStore(strBytesBlock.NewLoad(types.I64, lenPtr), resultLenPtr)
	strBytesBlock.NewStore(strBytesBlock.NewLoad(types.I64, lenPtr), resultCapPtr)

	strBytesBlock.NewStore(strBytesBlock.NewLoad(byteSliceType, result), retParam)
	strBytesBlock.NewRet(nil)
}

func GenRuntime() string {
	rtModule := ir.NewModule()

	exitDeclaration := rtModule.NewFunc("exit", types.Void, ir.NewParam("", types.I32))
	mallocDeclaration = rtModule.NewFunc("malloc", types.I8Ptr, ir.NewParam("", types.I64))
	freeDeclaration = rtModule.NewFunc("free", types.Void, ir.NewParam("", types.I8Ptr))
	memcpyDeclaration = rtModule.NewFunc(
		"llvm.memcpy.p0i8.p0i8.i64",
		types.Void,
		ir.NewParam("dest", types.I8Ptr),
		ir.NewParam("src", types.I8Ptr),
		ir.NewParam("len", types.I64),
		ir.NewParam("isvolatile", types.I1),
	)

	prevModule := module
	module = rtModule
	createPrimitiveMethods()
	module = prevModule

	panicMessageParam := ir.NewParam("message", types.I8Ptr)
	panicDeclaration = rtModule.NewFunc("panic", types.Void, panicMessageParam)
	panicBlock := panicDeclaration.NewBlock("")

	putsDeclaration = rtModule.NewFunc("puts", types.I32, ir.NewParam("", types.I8Ptr))
	panicBlock.NewCall(putsDeclaration, panicMessageParam)
	panicBlock.NewCall(exitDeclaration, constant.NewInt(types.I32, 1))
	panicBlock.NewRet(nil)

	return rtModule.String()
}

func Gen(m *ast.Module) string {
	module = ir.NewModule()
	module.SourceFilename = m.Path

	putsDeclaration = module.NewFunc("puts", types.I32, ir.NewParam("", types.I8Ptr))
	mallocDeclaration = module.NewFunc("malloc", types.I8Ptr, ir.NewParam("", types.I64))
	freeDeclaration = module.NewFunc("free", types.Void, ir.NewParam("", types.I8Ptr))
	memcpyDeclaration = module.NewFunc(
		"llvm.memcpy.p0i8.p0i8.i64",
		types.Void,
		ir.NewParam("dest", types.I8Ptr),
		ir.NewParam("src", types.I8Ptr),
		ir.NewParam("len", types.I64),
		ir.NewParam("isvolatile", types.I1),
	)
	panicDeclaration = module.NewFunc("panic", types.Void, ir.NewParam("message", types.I8Ptr))

	declarePrimitiveMethods()

	for _, statement := range m.Statements {
		genStatement(statement, m)
	}

	return module.String()
}
