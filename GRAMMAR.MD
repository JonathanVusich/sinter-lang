### Statement Types

`outerStmt => useStmt | classStmt | fnStmt | letStmt | enumStmt | traitStmt | traitImplStmt`

`stmt => forStmt | ifStmt | returnStmt | whileStmt | blockStmt | letStmt | expr`

_________________________________
### Outer Statement Definitions

`useStmt => "use" qualifiedIdent`

`classStmt => "inline"? "class" ident generics? params? (fnBody | ";")`

`fnStmt => "fn" ident generics? params? blockStmt`

`letStmt => "let" "mut"? ident ("=" expr)? ";"`

`enumStmt => "enum" ident generics? (enumBody | ";")`

`traitStmt => "trait" ident generics? (traitBody | ";")`

`traitImplStmt => "impl" pathTy "for" ty (fnBody | ";")`

`blockStmt => "{" stmt* "}"`

_________________________________
### Statement Definitions

`forStmt => "for" ident "in" expr blockStmt`

`ifStmt => "if" expr blockStmt ("else if" expr blockStmt)* ("else" blockStmt)?`

`returnStmt => "return" expr?`

`whileStmt => "while" expr blockStmt`

_________________________________
### Enum Definitions

`enumBody => "{" enumMember (, enumMember)* ,? "}"`

`enumMember => ident params? fnBody?`

_________________________________
### Trait Definitions

`traitBody => "{" fnAnyStmt* "}"`

_________________________________
### Function Definitions

`fnBody => "{" fnSelfStmt* "}"`

`fnAnyStmt => fnSelfStmt | fnSelfNoImplStmt`
_________________________________
### Parameter Definitions

`params => "(" param (, param)* ")"`

`selfParams => "(" selfParam (, param)* ")"`

`selfParam => "mut"? "self"`

`param => "mut"? ident ":" ty`