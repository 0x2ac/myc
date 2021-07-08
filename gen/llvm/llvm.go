package llvmgen

import (
	"fmt"
	"strconv"

	"github.com/kartiknair/myc/ast"
	"github.com/kartiknair/myc/lexer"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

func genType(t ast.Type) types.Type {
	switch t.(type) {
	case *ast.Primitive:
		return genPrimitive(t.(*ast.Primitive))
	case *ast.SliceType:
		st := t.(*ast.SliceType)
		// { buffer *ElType, length i64, capacity i64 }
		typ := types.NewStruct(types.NewPointer(genType(st.ElType)), types.I64, types.I64)

		printFuncName := fmt.Sprintf("myc__slice_%s_print", getTypeName(st.ElType))
		printFuncExists := false

		for _, f := range module.Funcs {
			if f.Name() == printFuncName {
				printFuncExists = true
			}
		}

		if !printFuncExists {
			createSlicePrint(st.ElType, typ)
		}

		return typ
	case *ast.StructType:
		return typespace[t.(*ast.StructType).Name]
	case *ast.PointerType:
		return types.NewPointer(genType(t.(*ast.PointerType).ElType))
	}

	panic("Type node has invalid static type.")
}

func genPrimitive(primitive *ast.Primitive) types.Type {
	switch primitive.Name {
	case "str":
		return types.NewStruct(types.I8Ptr, types.I64, types.I64)
	case "int":
		return types.I32
	case "float":
		return types.Double
	case "bool":
		return types.I1
	}

	panic("Invalid primitive type.")
}

var (
	module *ir.Module
	block  *ir.Block

	panicDeclaration  *ir.Func
	printfDeclaration *ir.Func

	namespace = make(map[string]value.Value)
	typespace = make(map[string]types.Type)
)

func getTypeName(t ast.Type) string {
	if p, ok := t.(*ast.Primitive); ok {
		return p.Name
	} else if s, ok := t.(*ast.StructType); ok {
		return s.Name
	} else if st, ok := t.(*ast.SliceType); ok {
		return fmt.Sprintf("slice_%s", getTypeName(st.ElType))
	}

	panic("Invalid type passed to `getTypeName`.")
}

func getPrintFuncForType(t ast.Type) *ir.Func {
	expectedName := fmt.Sprintf("myc__%s_print", getTypeName(t))

	for _, f := range module.Funcs {
		if f.Name() == expectedName {
			return f
		}
	}

	panic(fmt.Sprintf("Could not find print function for type: '%s'.", t.String()))
}

func genStatement(stmt ast.Statement) {
	switch stmt.(type) {
	case *ast.StructDeclaration:
		s := stmt.(*ast.StructDeclaration)

		var fields []types.Type
		for _, m := range s.Members {
			fields = append(fields, genType(m.Type))
		}

		t := types.NewStruct(fields...)
		typespace[s.Identifier.Lexeme] = t

		selfParam := ir.NewParam("", types.NewPointer(t))
		structPrintFunction := module.NewFunc(fmt.Sprintf("myc__%s_print", s.Identifier.Lexeme), types.Void, selfParam)
		printBlock := structPrintFunction.NewBlock("")

		initString := fmt.Sprintf("%s{\x00", s.Identifier.Lexeme)
		initStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString(initString))
		printBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
			types.NewArray(uint64(len(initString)), types.I8),
			initStringDef,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		))

		colonSpace := ": \x00"
		colonSpaceDef := module.NewGlobalDef("", constant.NewCharArrayFromString(colonSpace))

		commaSpace := ", \x00"
		commaSpaceDef := module.NewGlobalDef("", constant.NewCharArrayFromString(commaSpace))

		for i, m := range s.Members {
			fieldString := fmt.Sprintf("%s\x00", m.Identifier.Lexeme)
			fieldStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString(fieldString))
			printBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
				types.NewArray(uint64(len(fieldString)), types.I8),
				fieldStringDef,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			))
			printBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
				types.NewArray(uint64(len(colonSpace)), types.I8),
				colonSpaceDef,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			))

			member := printBlock.NewGetElementPtr(t, selfParam,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, int64(i)),
			)
			loadedMember := printBlock.NewLoad(genType(m.Type), member)

			if _, ok := loadedMember.Type().(*types.StructType); ok {
				printBlock.NewCall(getPrintFuncForType(m.Type), loadedMember.Src)
			} else {
				printBlock.NewCall(getPrintFuncForType(m.Type), loadedMember)
			}

			if i != len(s.Members)-1 {
				printBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
					types.NewArray(uint64(len(commaSpace)), types.I8),
					commaSpaceDef,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				))
			}
		}

		closeString := "}\x00"
		closeStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString(closeString))
		printBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
			types.NewArray(uint64(len(closeString)), types.I8),
			closeStringDef,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		))

		printBlock.NewRet(nil)
	case *ast.FunctionDeclaration:
		s := stmt.(*ast.FunctionDeclaration)
		irParams := []*ir.Param{}

		for _, param := range s.Parameters {
			irParams = append(
				irParams,
				ir.NewParam(
					param.Identifier.Lexeme,
					genType(param.Type),
				),
			)
		}

		var retTyp types.Type
		if s.ReturnType == nil {
			retTyp = types.Void
		} else {
			retTyp = genType(s.ReturnType)
		}

		fun := module.NewFunc(s.Identifier.Lexeme, retTyp, irParams...)
		funBlock := fun.NewBlock("")

		namespace[s.Identifier.Lexeme] = fun
		shadowed := make(map[string]value.Value)

		for i, param := range s.Parameters {
			variable := funBlock.NewAlloca(genType(param.Type))
			funBlock.NewStore(fun.Params[i], variable)

			if oldVal, ok := namespace[param.Identifier.Lexeme]; ok {
				shadowed[param.Identifier.Lexeme] = oldVal
			}

			namespace[param.Identifier.Lexeme] = variable
		}

		oldBlock := block
		block = funBlock
		for _, stmt := range s.Block.Statements {
			genStatement(stmt)
		}
		block = oldBlock

		for _, param := range s.Parameters {
			delete(namespace, param.Identifier.Lexeme)
			if oldVal, ok := shadowed[param.Identifier.Lexeme]; ok {
				namespace[param.Identifier.Lexeme] = oldVal
			}
		}

		if funBlock.Term == nil {
			funBlock.NewRet(nil)
		}
	case *ast.VariableDeclaration:
		s := stmt.(*ast.VariableDeclaration)
		variable := block.NewAlloca(genType(s.Type))
		block.NewStore(genExpression(s.Value), variable)
		//                           ^^^^^^^
		// TODO: Need assure that this is 0-value intialized based on the type.
		namespace[s.Identifier.Lexeme] = variable
	case *ast.ConstantDeclaration:
		s := stmt.(*ast.ConstantDeclaration)
		variable := block.NewAlloca(genType(s.Value.Type()))
		block.NewStore(genExpression(s.Value), variable)
		namespace[s.Identifier.Lexeme] = variable
	case *ast.IfStatement:
		s := stmt.(*ast.IfStatement)

		condition := genExpression(s.Condition)
		iftrueBlock := block.Parent.NewBlock("")
		iffalseBlock := block.Parent.NewBlock("")
		afterBlock := block.Parent.NewBlock("")
		block.NewCondBr(condition, iftrueBlock, iffalseBlock)

		block = iftrueBlock
		genStatement(&s.IfBlock)
		if block.Term == nil {
			block.NewBr(afterBlock)
		}

		block = iffalseBlock
		if s.ElseBlock != nil {
			genStatement(s.ElseBlock)
		}
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
		genStatement(&s.Block)
		conditionAgain := genExpression(s.Condition)
		block.NewCondBr(conditionAgain, loopBlock, afterBlock)

		block = afterBlock
	case *ast.PrintStatement:
		s := stmt.(*ast.PrintStatement)

		space := " \x00"
		spaceDef := module.NewGlobalDef("", constant.NewCharArrayFromString(space))

		lf := "\n\x00"
		lfDef := module.NewGlobalDef("", constant.NewCharArrayFromString(lf))

		for i, e := range s.Expressions {
			genned := genExpression(e)
			if _, ok := genned.Type().(*types.StructType); ok {
				block.NewCall(getPrintFuncForType(e.Type()), genned.(*ir.InstLoad).Src)
			} else {
				block.NewCall(getPrintFuncForType(e.Type()), genned)
			}

			if i != len(s.Expressions)-1 {
				block.NewCall(printfDeclaration, constant.NewGetElementPtr(
					types.NewArray(uint64(len(space)), types.I8),
					spaceDef,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				))
			}
		}

		block.NewCall(printfDeclaration, constant.NewGetElementPtr(
			types.NewArray(uint64(len(lf)), types.I8),
			lfDef,
			constant.NewInt(types.I32, 0),
			constant.NewInt(types.I32, 0),
		))
	case *ast.ReturnStatement:
		s := stmt.(*ast.ReturnStatement)
		block.NewRet(genExpression(s.Expression))
	case *ast.ExpressionStatement:
		s := stmt.(*ast.ExpressionStatement)
		genExpression(s.Expression)
	case *ast.BlockStatement:
		s := stmt.(*ast.BlockStatement)
		for _, stmt := range s.Statements {
			genStatement(stmt)
		}
	}
}

func genExpression(expr ast.Expression) value.Value {
	switch expr.(type) {
	case *ast.UnaryExpression:
		e := expr.(*ast.UnaryExpression)
		switch e.Operator.Type {
		case lexer.MINUS:
			return block.NewSub(constant.NewInt(types.I32, 0), genExpression(e.Value))
		}
	case *ast.BinaryExpression:
		e := expr.(*ast.BinaryExpression)
		switch e.Operator.Type {
		case lexer.EQUAL:
			// If assigning to a dereference we generate it's nested expression instead.
			if deref, ok := e.Left.(*ast.Dereference); ok {
				block.NewStore(genExpression(e.Right), genExpression(deref.Expression))
				return block.NewLoad(genType(e.Left.Type()), genExpression(deref.Expression))
			} else {
				expr := genExpression(e.Left)
				if load, ok := expr.(*ir.InstLoad); ok {
					block.NewStore(genExpression(e.Right), load.Src)
					return block.NewLoad(load.Type(), load.Src)
				} else {
					block.NewStore(genExpression(e.Right), expr)
					return block.NewLoad(expr.Type(), expr)
				}
			}
		case lexer.PLUS:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFAdd(genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewAdd(genExpression(e.Left), genExpression(e.Right))
		case lexer.MINUS:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFSub(genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewSub(genExpression(e.Left), genExpression(e.Right))
		case lexer.STAR:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFMul(genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewMul(genExpression(e.Left), genExpression(e.Right))
		case lexer.SLASH:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFDiv(genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewSDiv(genExpression(e.Left), genExpression(e.Right))
		case lexer.LESSER:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredOLT, genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewICmp(enum.IPredSLT, genExpression(e.Left), genExpression(e.Right))
		case lexer.GREATER:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredOGT, genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewICmp(enum.IPredSGT, genExpression(e.Left), genExpression(e.Right))
		case lexer.LESSER_EQUAL:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredOLE, genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewICmp(enum.IPredSLE, genExpression(e.Left), genExpression(e.Right))
		case lexer.GREATER_EQUAL:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredOGE, genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewICmp(enum.IPredSGE, genExpression(e.Left), genExpression(e.Right))
		case lexer.EQUAL_EQUAL:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredOEQ, genExpression(e.Left), genExpression(e.Right))
			}
			// ICmp works for both `int` and `bool`
			return block.NewICmp(enum.IPredEQ, genExpression(e.Left), genExpression(e.Right))
		case lexer.BANG_EQUAL:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFCmp(enum.FPredONE, genExpression(e.Left), genExpression(e.Right))
			}
			return block.NewICmp(enum.IPredNE, genExpression(e.Left), genExpression(e.Right))
		case lexer.AND_AND:
			oldBlock := block

			left := genExpression(e.Left)
			ifLeftTrueBlock := block.Parent.NewBlock("")
			otherwiseBlock := block.Parent.NewBlock("")

			block.NewCondBr(left, ifLeftTrueBlock, otherwiseBlock)

			block = ifLeftTrueBlock
			right := genExpression(e.Right)
			ifLeftTrueBlock.NewBr(otherwiseBlock)

			block = otherwiseBlock
			return block.NewPhi(ir.NewIncoming(constant.False, oldBlock), ir.NewIncoming(right, ifLeftTrueBlock))
		case lexer.OR_OR:
			left := genExpression(e.Left)
			right := genExpression(e.Right)
			return block.NewOr(left, right)
		}

	case *ast.CallExpression:
		e := expr.(*ast.CallExpression)
		args := []value.Value{}
		for _, arg := range e.Arguments {
			args = append(args, genExpression(arg))
		}

		var callee value.Value
		if varExpr, ok := e.Callee.(*ast.VariableExpression); ok {
			callee = namespace[varExpr.Identifier.Lexeme]
		} else {
			panic("Can only generate variables as calle.")
		}

		return block.NewCall(callee, args...)
	case *ast.VariableExpression:
		e := expr.(*ast.VariableExpression)
		resolvedVariable := namespace[e.Identifier.Lexeme]
		return block.NewLoad(resolvedVariable.Type().(*types.PointerType).ElemType, resolvedVariable)
	case *ast.GetExpression:
		e := expr.(*ast.GetExpression)
		st := e.Expression.Type().(*ast.StructType)

		memberIndex := 0
		for i, m := range st.Members {
			if m.Identifier.Lexeme == e.Identifier.Lexeme {
				memberIndex = i
			}
		}

		expr := genExpression(e.Expression)
		var gep *ir.InstGetElementPtr

		if loadInst, ok := expr.(*ir.InstLoad); ok {
			gep = block.NewGetElementPtr(
				typespace[st.Name],
				loadInst.Src,
				constant.NewInt(types.I32, int64(0)),
				constant.NewInt(types.I32, int64(memberIndex)),
			)
		} else {
			panic("internal error: get expression on non-load instruction.")
		}

		return block.NewLoad(genType(st.Members[memberIndex].Type), gep)
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

			message := "runtime-error: index out of bounds\x0A\x00"
			formatStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString(message))
			idxOutOfBoundsBlock.NewCall(panicDeclaration, constant.NewGetElementPtr(
				types.NewArray(uint64(len(message)), types.I8),
				formatStringDef,
				constant.NewInt(types.I32, 0),
				constant.NewInt(types.I32, 0),
			))
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
	case *ast.CompositeLiteral:
		e := expr.(*ast.CompositeLiteral)
		st := e.Typ.(*ast.StructType)
		t := typespace[st.Name]

		local := block.NewAlloca(t)

		if e.NamedInitializers != nil {
			for i, initializer := range *e.NamedInitializers {
				tmp := block.NewGetElementPtr(
					t,
					local,
					constant.NewInt(types.I32, int64(0)),
					constant.NewInt(types.I32, int64(i)),
				)
				block.NewStore(genExpression(initializer.Value), tmp)
			}
		} else {
			for i, initializer := range *e.UnnamedInitializers {
				tmp := block.NewGetElementPtr(
					t,
					local,
					constant.NewInt(types.I32, int64(0)),
					constant.NewInt(types.I32, int64(i)),
				)
				block.NewStore(genExpression(initializer), tmp)
			}
		}

		return block.NewLoad(t, local)
	case *ast.SliceLiteral:
		e := expr.(*ast.SliceLiteral)
		local := block.NewAlloca(genType(e.Type()))
		eltype := e.Type().(*ast.SliceType).ElType

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

		ptrmem := block.NewAlloca(genType(e.Type().(*ast.SliceType).ElType))
		block.NewStore(constant.NewInt(types.I64, int64(len(e.Expressions))), length)

		if len(e.Expressions) == 0 {
			block.NewStore(constant.NewInt(types.I64, 8), capacity)
			ptrmem.NElems = constant.NewInt(types.I32, 8)
		} else {
			block.NewStore(constant.NewInt(types.I64, int64(len(e.Expressions))), capacity)
			ptrmem.NElems = constant.NewInt(types.I32, int64(len(e.Expressions)))
		}

		block.NewStore(ptrmem, buffer)

		for i, value := range e.Expressions {
			loadedBuffer := block.NewLoad(types.NewPointer(genType(eltype)), buffer)
			pointerToElement := block.NewGetElementPtr(genType(eltype), loadedBuffer, constant.NewInt(types.I64, int64(i)))
			block.NewStore(genExpression(value), pointerToElement)
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
		switch e.LiteralType {
		case lexer.INT:
			parsedInt, _ := strconv.ParseInt(e.LiteralValue, 10, 32)
			tmp := block.NewAlloca(types.I32)
			block.NewStore(constant.NewInt(types.I32, parsedInt), tmp)
			return block.NewLoad(types.I32, tmp)
		case lexer.FLOAT:
			parsedFloat, _ := strconv.ParseFloat(e.LiteralValue, 64)
			tmp := block.NewAlloca(types.Double)
			block.NewStore(constant.NewFloat(types.Double, parsedFloat), tmp)
			return block.NewLoad(types.Double, tmp)
		case lexer.TRUE:
			tmp := block.NewAlloca(types.I1)
			block.NewStore(constant.NewInt(types.I1, 1), tmp)
			return block.NewLoad(types.I1, tmp)
		case lexer.FALSE:
			tmp := block.NewAlloca(types.I1)
			block.NewStore(constant.NewInt(types.I1, 0), tmp)
			return block.NewLoad(types.I1, tmp)
		}
	}

	panic(fmt.Sprintf("Expression node has invalid static type. %T", expr))
}

func createSlicePrint(elType ast.Type, llvmType types.Type) {
	openBracket := "[\x00"
	openBracketDef := module.NewGlobalDef("", constant.NewCharArrayFromString(openBracket))
	commaSpace := ", \x00"
	commaSpaceDef := module.NewGlobalDef("", constant.NewCharArrayFromString(commaSpace))
	closeBracket := "]\x00"
	closeBracketDef := module.NewGlobalDef("", constant.NewCharArrayFromString(closeBracket))

	param := ir.NewParam("", types.NewPointer(llvmType))
	f := module.NewFunc(
		fmt.Sprintf("myc__slice_%s_print", getTypeName(elType)),
		types.Void,
		param,
	)
	b := f.NewBlock("")

	idxVar := b.NewAlloca(types.I64)
	b.NewStore(constant.NewInt(types.I64, 0), idxVar)
	idx := b.NewLoad(types.I64, idxVar)
	length := b.NewGetElementPtr(llvmType, param,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 1),
	)
	loadedLength := b.NewLoad(types.I64, length)

	buffer := b.NewGetElementPtr(
		llvmType,
		param,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	loadedBuffer := b.NewLoad(types.NewPointer(genType(elType)), buffer)

	b.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(uint64(len(openBracket)), types.I8),
		openBracketDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))

	comparison := b.NewICmp(enum.IPredULT, idx, loadedLength)
	loopBlock := b.Parent.NewBlock("")
	nocommaPrintBlock := b.Parent.NewBlock("")
	afterBlock := b.Parent.NewBlock("")
	b.NewCondBr(comparison, loopBlock, afterBlock)

	idx = loopBlock.NewLoad(types.I64, idxVar)
	valueAtIdx := loopBlock.NewGetElementPtr(genType(elType), loadedBuffer, idx)
	if _, ok := genType(elType).(*types.StructType); ok {
		loopBlock.NewCall(getPrintFuncForType(elType), valueAtIdx)
	} else {
		loadedValue := loopBlock.NewLoad(genType(elType), valueAtIdx)
		loopBlock.NewCall(getPrintFuncForType(elType), loadedValue)
	}
	loopBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(uint64(len(commaSpace)), types.I8),
		commaSpaceDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))

	incrementedIdx := loopBlock.NewAdd(idx, constant.NewInt(types.I64, 1))
	loopBlock.NewStore(incrementedIdx, idxVar)
	comparisonAgain := loopBlock.NewICmp(enum.IPredEQ, incrementedIdx, loopBlock.NewSub(loadedLength, constant.NewInt(types.I64, 1)))
	loopBlock.NewCondBr(comparisonAgain, nocommaPrintBlock, loopBlock)

	idx = nocommaPrintBlock.NewLoad(types.I64, idxVar)
	valueAtIdx = nocommaPrintBlock.NewGetElementPtr(genType(elType), loadedBuffer, idx)
	if _, ok := genType(elType).(*types.StructType); ok {
		nocommaPrintBlock.NewCall(getPrintFuncForType(elType), valueAtIdx)
	} else {
		loadedValue := nocommaPrintBlock.NewLoad(genType(elType), valueAtIdx)
		nocommaPrintBlock.NewCall(getPrintFuncForType(elType), loadedValue)
	}
	nocommaPrintBlock.NewBr(afterBlock)

	afterBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(uint64(len(closeBracket)), types.I8),
		closeBracketDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))
	afterBlock.NewRet(nil)
}

func createPrimitivePrintFunctions() {
	var param *ir.Param
	var fmtDef *ir.Global

	// int
	param = ir.NewParam("", genType(&ast.Primitive{Name: "int"}))
	intf := module.NewFunc("myc__int_print", types.Void, param)
	ib := intf.NewBlock("")
	fmtDef = module.NewGlobalDef("", constant.NewCharArrayFromString("%d\x00"))
	ib.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(3, types.I8),
		fmtDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	), param)
	ib.NewRet(nil)

	// float
	param = ir.NewParam("", genType(&ast.Primitive{Name: "float"}))
	floatf := module.NewFunc("myc__float_print", types.Void, param)
	fb := floatf.NewBlock("")
	fmtDef = module.NewGlobalDef("", constant.NewCharArrayFromString("%.16g\x00"))
	fb.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(6, types.I8),
		fmtDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	), param)
	fb.NewRet(nil)

	// bool
	param = ir.NewParam("", genType(&ast.Primitive{Name: "bool"}))
	boolf := module.NewFunc("myc__bool_print", types.Void, param)
	entryBlock := boolf.NewBlock("entry")
	trueBlock := boolf.NewBlock("iftrue")
	falseBlock := boolf.NewBlock("iffalse")
	entryBlock.NewCondBr(param, trueBlock, falseBlock)

	trueStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString("true\x00"))
	falseStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString("false\x00"))

	trueBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(uint64(5), types.I8),
		trueStringDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))
	trueBlock.NewRet(nil)

	falseBlock.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(uint64(6), types.I8),
		falseStringDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))
	falseBlock.NewRet(nil)

	// str
	param = ir.NewParam("", types.NewPointer(genType(&ast.Primitive{Name: "str"})))
	strf := module.NewFunc("myc__str_print", types.Void, param)
	sb := strf.NewBlock("")
	buffer := sb.NewGetElementPtr(
		genType(&ast.Primitive{Name: "str"}),
		param,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	)
	loadedBuffer := sb.NewLoad(types.I8Ptr, buffer)
	fmtDef = module.NewGlobalDef("", constant.NewCharArrayFromString("%s\x00"))
	sb.NewCall(printfDeclaration, constant.NewGetElementPtr(
		types.NewArray(3, types.I8),
		fmtDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	), loadedBuffer)
	sb.NewRet(nil)
}

func Gen(statements []ast.Statement) string {
	module = ir.NewModule()

	exitDeclaration := module.NewFunc("exit", types.Void, ir.NewParam("", types.I32))

	printfDeclaration = module.NewFunc("printf", types.I32, ir.NewParam("", types.I8Ptr))
	printfDeclaration.Sig.Variadic = true

	createPrimitivePrintFunctions()

	panicMessageParam := ir.NewParam("message", types.I8Ptr)
	panicDeclaration = module.NewFunc("panic", types.Void, panicMessageParam)
	panicBlock := panicDeclaration.NewBlock("")
	panicBlock.NewCall(printfDeclaration, panicMessageParam)
	panicBlock.NewCall(exitDeclaration, constant.NewInt(types.I32, 1))
	panicBlock.NewRet(nil)

	cmain := module.NewFunc("main", types.I32)
	block = cmain.NewBlock("main_block")

	for _, statement := range statements {
		genStatement(statement)
	}

	if block.Term == nil {
		block.NewRet(constant.NewInt(types.I32, 0))
	}

	return module.String()
}
