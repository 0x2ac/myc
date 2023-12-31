The grammar for `myc` is currently pretty small. The idea is to grow the language slowly improving funcionality but trying to keep the grammar easy (and therefore keeping the language easier to parse).

The current grammar is written in the format used by the [`owl`](https://github.com/ianh/owl/) parser generator; therefore the `.operators` notation and `: Name` shorthand.

```
Program = Declaration+

Declaration =
	FunctionDeclaration
	StructDeclaration
	VariableDeclaration
	ConstantDeclaration

FunctionDeclaration = 'fun' identifier ParameterList (Type)? Block
ParameterList = ['(' ((identifier Type) (',' identifier Type)*)? ')' ]

StructDeclaration = 'struct' identifier '{ StructDefinition+ '}'
StructMember = identifier Type ';'

VariableDeclaration = 'var' identifier (Type)? ('=' Expression)? ';'
ConstantDeclaration = 'const' identifier '=' Expression ';'

Statement =
	VariableDeclaration
	ConstantDeclaration
	'print' (Expression (',' Expression)*) ';'      : PrintStatement
	'return' (Expression)? ';'                      : ReturnStatement
	Expression ';'                                  : ExpressionStatement
	Block                                           : BlockStatement

Block = ['{' Statement+ '}']

Expression =
	identifier : VariableExpression
	GetExpression
	CompositeLiteral
	Literal

  .operators prefix
	'-' : Negate

  .operators infix left
	'=' : Assign
	'*' : Multiply
	'/' : Divide
	'%' : Modulus

  .operators infix left
	'+' : Add
	'-' : Subtract

  .operators postfix
	['(' (Expression (',' Expression)*)? ')'] : CallExpression

GetExpression =
	Expression '.' identifier

CompositeLiteral =
	Type '{' NamedInitializers | UnnamedInitializers '}'

NamedInitializers =
	(Identifier ':' Expression (',' Identifier ':' Expression)*)?

UnnamedInitializers =
	(Expression (',' Expression)*)?

Literal =
	integer : Integer

Type =
	PrimitiveType : Primitive

PrimitiveType = 'int' | 'float'
```
