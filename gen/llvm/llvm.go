package llvmgen

import (
	"strconv"

	"github.com/kartiknair/myc/ast"
	"github.com/kartiknair/myc/lexer"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

func genType(t ast.Type) types.Type {
	switch t.(type) {
	case *ast.Primitive:
		return genPrimitive(t.(*ast.Primitive))
	case *ast.StructType:
		return typespace[t.(*ast.StructType).Name]
	}

	panic("Type node has invalid static type.")
}

func genPrimitive(primitive *ast.Primitive) types.Type {
	switch primitive.Name {
	case "int":
		return types.I32
	case "float":
		return types.Double
	}

	panic("Invalid primitive type.")
}

var module *ir.Module
var printfDeclaration *ir.Func
var namespace = make(map[string]value.Value)
var typespace = make(map[string]types.Type)

func getFormatStringForType(t ast.Type) string {
	if primitive, ok := t.(*ast.Primitive); ok {
		switch primitive.Name {
		case "int":
			return "%d"
		case "float":
			return "%f"
		}
	} else if _, ok := t.(*ast.StructType); ok {
		return "%p"
	}

	panic("Invalid type passed to `getFormatStringForType`.")
}

func genStatement(stmt ast.Statement, block *ir.Block) {
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

		for i, param := range s.Parameters {
			variable := funBlock.NewAlloca(genType(param.Type))
			funBlock.NewStore(fun.Params[i], variable)
			namespace[param.Identifier.Lexeme] = variable
		}

		for _, stmt := range s.Block.Statements {
			genStatement(stmt, funBlock)
		}

		for _, param := range s.Parameters {
			delete(namespace, param.Identifier.Lexeme)
		}

		if funBlock.Term == nil {
			funBlock.NewRet(nil)
		}
	case *ast.VariableDeclaration:
		s := stmt.(*ast.VariableDeclaration)
		variable := block.NewAlloca(genType(s.Type))
		block.NewStore(genExpression(s.Value, block), variable)
		//                           ^^^^^^^
		// TODO: Need assure that this is 0-value intialized based on the type.
		namespace[s.Identifier.Lexeme] = variable
	case *ast.ConstantDeclaration:
		s := stmt.(*ast.ConstantDeclaration)
		variable := block.NewAlloca(genType(s.Value.Type()))
		block.NewStore(genExpression(s.Value, block), variable)
		namespace[s.Identifier.Lexeme] = variable
	case *ast.PrintStatement:
		s := stmt.(*ast.PrintStatement)

		formatString := ""
		printfArgs := []value.Value{}
		for i, expr := range s.Expressions {
			formatString += getFormatStringForType(expr.Type())

			e := genExpression(expr, block)
			if _, ok := e.Type().(*types.StructType); ok {
				printfArgs = append(printfArgs, e.(*ir.InstLoad).Src)
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
		block.NewRet(genExpression(s.Expression, block))
	case *ast.ExpressionStatement:
		s := stmt.(*ast.ExpressionStatement)
		genExpression(s.Expression, block)
	case *ast.BlockStatement:
		s := stmt.(*ast.BlockStatement)
		for _, stmt := range s.Statements {
			genStatement(stmt, block)
		}
	}
}

func genExpression(expr ast.Expression, block *ir.Block) value.Value {
	switch expr.(type) {
	case *ast.UnaryExpression:
		e := expr.(*ast.UnaryExpression)
		switch e.Operator.Type {
		case lexer.MINUS:
			return block.NewSub(constant.NewInt(types.I32, 0), genExpression(e.Value, block))
		}
	case *ast.BinaryExpression:
		e := expr.(*ast.BinaryExpression)
		switch e.Operator.Type {
		case lexer.EQUAL:
			block.NewStore(genExpression(e.Right, block), genExpression(e.Left, block))
			return block.NewLoad(genType(e.Left.Type()), genExpression(e.Left, block))
		case lexer.PLUS:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFAdd(genExpression(e.Left, block), genExpression(e.Right, block))
			}
			return block.NewAdd(genExpression(e.Left, block), genExpression(e.Right, block))
		case lexer.MINUS:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFSub(genExpression(e.Left, block), genExpression(e.Right, block))
			}
			return block.NewSub(genExpression(e.Left, block), genExpression(e.Right, block))
		case lexer.STAR:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFMul(genExpression(e.Left, block), genExpression(e.Right, block))
			}
			return block.NewMul(genExpression(e.Left, block), genExpression(e.Right, block))
		case lexer.SLASH:
			if e.Left.Type().Equals(&ast.Primitive{Name: "float"}) {
				return block.NewFDiv(genExpression(e.Left, block), genExpression(e.Right, block))
			}
			return block.NewSDiv(genExpression(e.Left, block), genExpression(e.Right, block))
		}
	case *ast.CallExpression:
		e := expr.(*ast.CallExpression)
		args := []value.Value{}
		for _, arg := range e.Arguments {
			args = append(args, genExpression(arg, block))
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

		expr := genExpression(e.Expression, block)
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
				block.NewStore(genExpression(initializer.Value, block), tmp)
			}
		} else {
			for i, initializer := range *e.UnnamedInitializers {
				tmp := block.NewGetElementPtr(
					t,
					local,
					constant.NewInt(types.I32, int64(0)),
					constant.NewInt(types.I32, int64(i)),
				)
				block.NewStore(genExpression(initializer, block), tmp)
			}
		}

		return block.NewLoad(t, local)
	case *ast.Literal:
		e := expr.(*ast.Literal)
		switch e.LiteralType {
		case lexer.INT:
			parsedInt, _ := strconv.ParseInt(e.LiteralValue, 10, 32)
			return constant.NewInt(types.I32, parsedInt)
		case lexer.FLOAT:
			parsedFloat, _ := strconv.ParseFloat(e.LiteralValue, 64)
			return constant.NewFloat(types.Double, parsedFloat)
		}
	}

	panic("Expression node has invalid static type.")
}

func Gen(statements []ast.Statement) string {
	module = ir.NewModule()

	printfDeclaration = module.NewFunc("printf", types.I32, ir.NewParam("format", types.I8Ptr))
	printfDeclaration.Sig.Variadic = true

	cmain := module.NewFunc("main", types.I32)
	cmainBlock := cmain.NewBlock("main_block")

	for _, statement := range statements {
		genStatement(statement, cmainBlock)
	}

	cmainBlock.NewRet(constant.NewInt(types.I32, 0))

	return module.String()
}
