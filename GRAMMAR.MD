### Statement Types

`outerStmt => useStmt | classStmt | fnStmt | letStmt | enumStmt | traitStmt | traitImplStmt`

`stmt => forStmt | ifStmt | returnStmt | whileStmt | blockStmt | letStmt | expression`

_________________________________
### Outer Statement Definitions

`useStmt => "use" qualifiedIdent`

`classStmt => "inline"? "class" ident genericDecls? params? ( objectFns | ";" )`

`fnStmt => fnHead params fnTail`

`letStmt => "let" "mut"? ident ( "=" expression )? ";"`

`enumStmt => "enum" ident genericDecls? enumBody ( objectFns | ";" )`

`traitStmt => "trait" ident genericDecls? ( traitBody | ";" )`

`traitImplStmt => "impl" qualifiedIdent genericCallsite? "for" ty ( objectFns | ";" )`

`blockStmt => "{" stmt* "}"`

_________________________________
### Statement Definitions

`forStmt => "for" ident "in" expression blockStmt`

`ifStmt => "if" expression blockStmt ( "else if" expression blockStmt )* ( "else" blockStmt )?`

`returnStmt => "return" expression?`

`whileStmt => "while" expression blockStmt`

_________________________________
### Expression Definitions

`expression => closure | assignment`

`closure => "(" ( ident ( , ident )* )? ")" "=>" ( blockStmt | expression )`

`assignment => logical_or ( ( "=" ) logical_or )*`

`logical_or => logical_and ( "||" logical_and )*`

`logical_and => bitwise_inclusive_or ( "&&" bitwise_inclusive_or )*`

`bitwise_inclusive_or => bitwise_exclusive_or ( "|" bitwise_exclusive_or )*`

`bitwise_exclusive_or => bitwise_and ( "^" bitwise_and )*`

`bitwise_and => equality ( "&" equality)*`

`equality => relational ( ( "==" | "!=" ) relational )*`

`relational => shift ( ( "<" | ">" | "<=" | ">=" ) shift )*`

`shift => additive ( ( "<<" | ">>" | ">>>" ) additive )*`

`additive => multiplicative ( ( "+" | "-" ) multiplicative )*`

`multiplicative => unary ( ( "*" | "/" | "%" ) unary )*`

`unary => ( "+" | "-" | "~" | "!" ) unary | call`

`call => primary ( fnCall | index | "." ident )*`

`fnCall => ( "::" genericCallsite ) "(" args? ")"`

`index => "[" expression "]"`

`primary => literal | "self" | path | "(" expression ")"`;

_________________________________
### Enum Definitions

`enumBody => "(" enumMember (, enumMember)* ,? ")"`

`enumMember => ident params? objectFns?`

_________________________________
### Trait Definitions

`traitBody => "{" fnTraitStmt* "}"`

_________________________________
### Function Definitions
`fnHead => "fn" ident genericDecls?`

`fnTail => ("=>" ty)? blockStmt`

`fnTraitStmt => fnSelfStmt | fnSelfNoImplStmt`

`fnSelfStmt => fnHead selfParams  ` 

`fnSelfNoImplStmt => "fn" ident selfParams ";"`

_________________________________
### Generic Definitions

`genericDecls => "<" generic ( , generic )* ">"`

`genericDecl => ident ( ":" traitBoundType )?`

`genericCallsite => "<" ty ( , ty )* ">"`

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

`anyType => basicType | arrayType | traitBoundType | closureType`

`basicType => "bool" | "None" | "u8" | "u16" | "u64" | 
              "i8" | "i16" | "i32" | i64" | "f32" | "f64"`

`arrayType => "[" ty "]"`

`traitBoundType => pathType ( "+" pathType )*`

`pathType => qualifiedIdent genericDecls?`

`closureType => "(" (ty ( , ty )* )? ")" "=>" ( "(" closureType ")" | (basicType | arrayType | path) )`


_________________________________
### General Definitions

`path => ident ( "::" ( genericCallsite "::" | ident ) )*`

`qualifiedIdent => ident ("::" ident)*`

`objectFns => "{" fnSelfStmt* "}"`







