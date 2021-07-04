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
	case *ast.StructType:
		return typespace[t.(*ast.StructType).Name]
	case *ast.PointerType:
		return types.NewPointer(genType(t.(*ast.PointerType).ElType))
	}

	panic("Type node has invalid static type.")
}

func genPrimitive(primitive *ast.Primitive) types.Type {
	switch primitive.Name {
	case "int":
		return types.I32
	case "float":
		return types.Double
	case "bool":
		return types.I1
	}

	panic("Invalid primitive type.")
}

var module *ir.Module
var block *ir.Block
var printfDeclaration *ir.Func
var boolToStringDeclaration *ir.Func
var namespace = make(map[string]value.Value)
var typespace = make(map[string]types.Type)

func getFormatStringForType(t ast.Type) string {
	if primitive, ok := t.(*ast.Primitive); ok {
		switch primitive.Name {
		case "int":
			return "%d"
		case "float":
			return "%f"
		case "bool":
			return "%s"
		}
	} else if _, ok := t.(*ast.StructType); ok {
		return "%p"
	}

	panic("Invalid type passed to `getFormatStringForType`.")
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
	case *ast.PrintStatement:
		s := stmt.(*ast.PrintStatement)

		formatString := ""
		printfArgs := []value.Value{}
		for i, expr := range s.Expressions {
			formatString += getFormatStringForType(expr.Type())

			e := genExpression(expr)
			if _, ok := e.Type().(*types.StructType); ok {
				printfArgs = append(printfArgs, e.(*ir.InstLoad).Src)
			} else if itype, ok := e.Type().(*types.IntType); ok && itype.BitSize == 1 {
				stringed := block.NewCall(boolToStringDeclaration, e)
				// I1 type gets extended while printing to print a clean 0 or 1.
				//
				// TODO: implement a simple function in LLVM to convert I1s to
				// "true"/"false" strings and print them instead.
				printfArgs = append(printfArgs, stringed)
			} else {
				printfArgs = append(printfArgs, e)
			}

			if i != len(s.Expressions)-1 {
				formatString += " "
			} else {
				formatString += "\x0A\x00"
				// Null-terminate and add trailing LF.
			}
		}

		formatStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString(formatString))
		printfArgs = append(
			[]value.Value{
				constant.NewGetElementPtr(
					types.NewArray(uint64(len(formatString)), types.I8),
					formatStringDef,
					constant.NewInt(types.I32, 0),
					constant.NewInt(types.I32, 0),
				),
			},
			printfArgs...,
		)

		block.NewCall(
			printfDeclaration,
			printfArgs...,
		)
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
			gep = block.NewGetElementPtr(
				typespace[st.Name],
				expr,
				constant.NewInt(types.I32, int64(0)),
				constant.NewInt(types.I32, int64(memberIndex)),
			)
		}

		return block.NewLoad(genType(st.Members[memberIndex].Type), gep)
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

func Gen(statements []ast.Statement) string {
	module = ir.NewModule()

	printfDeclaration = module.NewFunc("printf", types.I32, ir.NewParam("format", types.I8Ptr))
	printfDeclaration.Sig.Variadic = true

	boolToStringParam := ir.NewParam("b", types.I1)
	boolToStringDeclaration = module.NewFunc("boolToString", types.I8Ptr, boolToStringParam)
	boolToStringEntryBlock := boolToStringDeclaration.NewBlock("entry")
	boolToStringTrueBlock := boolToStringDeclaration.NewBlock("iftrue")
	boolToStringFalseBlock := boolToStringDeclaration.NewBlock("iffalse")
	boolToStringEntryBlock.NewCondBr(boolToStringParam, boolToStringTrueBlock, boolToStringFalseBlock)

	trueStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString("true\x00"))
	falseStringDef := module.NewGlobalDef("", constant.NewCharArrayFromString("false\x00"))

	boolToStringTrueBlock.NewRet(constant.NewGetElementPtr(
		types.NewArray(uint64(5), types.I8),
		trueStringDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))

	boolToStringFalseBlock.NewRet(constant.NewGetElementPtr(
		types.NewArray(uint64(6), types.I8),
		falseStringDef,
		constant.NewInt(types.I32, 0),
		constant.NewInt(types.I32, 0),
	))

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
