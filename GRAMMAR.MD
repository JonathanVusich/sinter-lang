### Statement Types

`outerStmt => useStmt | classStmt | fnStmt | letStmt | enumStmt | traitStmt | traitImplStmt`

`stmt => forStmt | ifStmt | returnStmt | whileStmt | blockStmt | letStmt | expr`

_________________________________
### Outer Statement Definitions

`useStmt => "use" qualifiedIdent`

`classStmt => "inline"? "class" ident generics? params? ( objectFns | ";" )`

`fnStmt => fnHead params fnTail`

`letStmt => "let" "mut"? ident ( "=" expr )? ";"`

`enumStmt => "enum" ident generics? ( enumBody | ";" )`

`traitStmt => "trait" ident generics? ( traitBody | ";" )`

`traitImplStmt => "impl" pathTy "for" ty ( objectFns | ";" )`

`blockStmt => "{" stmt* "}"`

_________________________________
### Statement Definitions

`forStmt => "for" ident "in" expr blockStmt`

`ifStmt => "if" expr blockStmt ( "else if" expr blockStmt )* ( "else" blockStmt )?`

`returnStmt => "return" expr?`

`whileStmt => "while" expr blockStmt`

_________________________________
### Expression Definitions

`expr => closure | assignment`

`closure => "(" ( ident ( , ident )* )? ")" "=>" ( blockStmt | expr )`

`assignment => logical_or ( ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "^=" | "|=" | "<<=" | ">>=" | ">>>=" ) logical_or )*`

`logical_or => logical_and ( "||" logical_and )*`

`logical_and => bitwise_inclusive_or ( "&&" bitwise_inclusive_or )*`

`bitwise_inclusive_or => bitwise_exclusive_or ( "|" bitwise_exclusive_or )*`

`bitwise_exclusive_or => bitwise_and ( "^" bitwise_and )*`

`bitwise_and => equality ( "&" equality )*`

`equality => relational ( ( "==" | "!=" ) relational )*`

`relational => shift ( ( "<" | ">" | "<=" | ">=" ) shift )*`

`shift => additive ( ( "<<" | ">>" | ">>>" ) additive )*`

`additive => multiplicative ( ( "+" | "-" ) multiplicative )*`

`multiplicative => unary ( ( "*" | "/" | "%" ) unary )*`

`unary => ( "+" | "-" | "~" | "!" ) unary | call`

`call => primary ( "(" args? ")" ) | ( "[" expression "]" ) | ( "." ident )*`

`primary => literal | "self" | ident | "(" expression ")"`

_________________________________
### Enum Definitions

`enumBody => "{" enumMember (, enumMember)* ,? "}"`

`enumMember => ident params? objectFns?`

_________________________________
### Trait Definitions

`traitBody => "{" fnTraitStmt* "}"`

_________________________________
### Function Definitions
`fnHead => "fn" ident generics?`

`fnTail => ("=>" ty)? blockStmt`

`fnTraitStmt => fnSelfStmt | fnSelfNoImplStmt`

`fnSelfStmt => fnHead selfParams  ` 

`fnSelfNoImplStmt => "fn" ident selfParams ";"`

_________________________________
### Generic Definitions

`generics => "<" generic (, generic)* ">"`

`generic => ident (":" ty)?`

`genericTys => "<" path (, path)* ">"`

_________________________________
### Parameter Definitions

`params => "(" param (, param)* ")"`

`selfParams => "(" selfParam (, param)* ")"`

`selfParam => "mut"? "self"`

`param => "mut"? ident ":" ty`

_________________________________
### Argument Definitions

`args => arg ( , arg )*`

`arg => expression`

_________________________________
### Type Definitions

`literal => "true" | "false" | "None" | number | string`

`ty => anyType ("|" anyType)*`

`anyType => basicType | arrayType | pathType | closureType`

`basicType => "bool" | "None" | "u8" | "u16" | "u64" | 
              "i8" | "i16" | "i32" | i64" | "f32" | "f64"`

`arrayType => "[" ty "]"`

`pathType => path ("+" path)*`

`closureType => "(" (ty (, ty)*)? ")" "=>" ( "(" closureType ")" | (basicType | arrayType | pathType) )`


_________________________________
### General Definitions

`path => qualifiedIdent generics?`

`qualifiedIdent => ident ("::" ident)*`

`objectFns => "{" fnSelfStmt* "}"`







