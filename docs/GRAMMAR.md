The grammar for `myc` is currently pretty small. The idea is to grow the language slowly improving funcionality but trying to keep the grammar easy (and therefore keeping the language easier to parse).

Here is the full grammar in an EBNF-like format (the colon on the right of a match arm is short-form for a new rule based on that match):

```
Program = Statement+

Statement =
	'var' identifier (Type)? ('=' Expression)? ';'               : VariableDeclaration
	'const' identifier '=' (Expression ((',') Expression)*) ';'  : ConstantDeclaration
	'print' Expression ';'                                       : PrintStatement
	Expression ';'                                               : ExpressionStatement
	Block                                                        : BlockStatement

Block = ['{' Statement+ '}']

Expression =
	Literal : Literal

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

Literal =
	integer : Integer

Type =
	PrimitiveType : Primitive

PrimitiveType = 'int' | 'float'
```
