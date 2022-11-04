### Statement Types

`outerStmt => useStmt | classStmt | fnStmt | letStmt | enumStmt | traitStmt | traitImplStmt`

`stmt => forStmt | ifStmt | returnStmt | whileStmt | blockStmt | letStmt | expr`

_________________________________
### Outer Statement Definitions

`useStmt => "use" qualifiedIdent`

`classStmt => "inline"? "class" ident generics? params? (objectFns | ";")`

`fnStmt => fnHead params fnTail`

`letStmt => "let" "mut"? ident ("=" expr)? ";"`

`enumStmt => "enum" ident generics? (enumBody | ";")`

`traitStmt => "trait" ident generics? (traitBody | ";")`

`traitImplStmt => "impl" pathTy "for" ty (objectFns | ";")`

`blockStmt => "{" stmt* "}"`

_________________________________
### Statement Definitions

`forStmt => "for" ident "in" expr blockStmt`

`ifStmt => "if" expr blockStmt ("else if" expr blockStmt)* ("else" blockStmt)?`

`returnStmt => "return" expr?`

`whileStmt => "while" expr blockStmt`

_________________________________
### Statement Definitions

`exprs => expr (, expr)*`

`expr => assignment`

`assignment => `

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
### Type Definitions

`literal => "true" | "false" | "None" | number | string`

`ty => basicType | "None" | arrayType | pathType`

`basicType => "u8" | "u16" | "u64" | "i8" | "i16" | "i32" | i64" | "f32" | "f64"`

`arrayType => "[" ty "]"`

`pathType => path (unionType | traitBound)?`

`unionType => ("|" ty)*`

`traitBound => ("+ ty)*`

_________________________________
### General Definitions

`path => qualifiedIdent genericTys?`

`qualifiedIdent => ident ("::" ident)*`

`objectFns => "{" fnSelfStmt* "}"`







