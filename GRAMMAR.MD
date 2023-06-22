### Notes on grammar decisions
1. Enums and classes include methods in the body of each type to avoid having to reconcile multiple impl blocks for the same class. It also makes generic parameters easier to keep track of since the declared generic params are the ones that are global to class or enum methods.
2. Class fields are delimited by semicolons. This makes parsing function bodies easier and avoids forced ordering for parsing to succeed.
3. 

### Statement Types

`outerStmt => useStmt | classStmt | fnStmt | constLetStmt | enumStmt | traitStmt | traitImplStmt`

`stmt => letStmt | forStmt | whileStmt | blockStmt | returnStmt | ifStmt | expression`

_________________________________
### Outer Statement Definitions

`useStmt => "use" qualifiedIdent ";"`

`classStmt => "ref"? "class" ident genericParams? ( "{" (field ",")* fnSelfStmt* "}" ) | ";"`

`fnStmt => fnHead params fnTail`

`constLetStmt => "let" ident ( ":" ty )? "=" expression ";"`

`enumStmt => "enum" ident genericParams? ( "{" (enumMember ",")* fnSelfStmt* "}" ) | ";"`

`traitStmt => "trait" ident genericParams? ( traitBody | ";" )`

`traitImplStmt => "impl" pathType "for" ty ( objectFns | ";" )`

`blockStmt => "{" stmt* "}"`

_________________________________
### Statement Definitions
`letStmt => "let" "mut"? ident ( ":" ty )? ( "=" expression )? ";"`

`forStmt => "for" ident "in" expression blockStmt`

`ifStmt => "if" expression blockStmt ( "else if" expression blockStmt )* ( "else" blockStmt )?`

`returnStmt => "return" expression?`

`whileStmt => "while" expression blockStmt`

`matchStmt => "match" expression matchBlock`

_________________________________
### Expression Definitions

`expression => closure | assignment`

`closure => "(" ( ident ( , ident )* )? ")" "=>" ( blockStmt | expression )`

`assignment => logicalOr ( ( "=" ) logicalOr )*`

`logicalOr => logicalAnd ( "||" logicalAnd )*`

`logicalAnd => bitwiseInclusiveOr ( "&&" bitwiseInclusiveOr )*`

`bitwiseInclusiveOr => bitwiseExclusiveOr ( "|" bitwiseExclusiveOr )*`

`bitwiseExclusiveOr => bitwiseAnd ( "^" bitwiseAnd )*`

`bitwiseAnd => equality ( "&" equality)*`

`equality => relational ( ( "==" | "!=" ) relational )*`

`relational => shift ( ( "<" | ">" | "<=" | ">=" ) shift )*`

`shift => additive ( ( "<<" | ">>" | ">>>" ) additive )*`

`additive => multiplicative ( ( "+" | "-" ) multiplicative )*`

`multiplicative => unary ( ( "*" | "/" | "%" ) unary )*`

`unary => ( "+" | "-" | "~" | "!" ) unary | call`

`call => primary ( fnCall | index | "." ident )*`

`fnCall => ( "::" genericCallsite )? "(" args? ")"`

`index => "[" expression "]"`

`primary => literal | "self" | path | "(" expression ")"`;

_________________________________
### Enum Definitions

`enumMember => ident params? ("{" fnSelfStmt* "}")?`

_________________________________
### Trait Definitions

`=> "{" fnTraitStmt* "}"`

_________________________________
### Function Definitions
`fnHead => "fn" ident genericParams?`

`fnTail => ( "=>" ty )? blockStmt`

`fnTailNoImpl => ( "=>" ty )? ";"`

`fnTraitStmts => "{" fnTraitStmt* "}"` 

`fnTraitStmt => fnSelfStmt | fnSelfNoImplStmt`

`fnSelfStmt => fnHead selfParams fnTail` 

`fnSelfNoImplStmt => fnHead selfParams fnTailNoImpl";"`

_________________________________
### Generic Definitions

`generics => "<" ty ( , ty )* ">"`

`genericParams => "<" genericParam ( , genericParam )* ">"`

`genericParam => ident ( ":" traitBoundType )`

_________________________________
### Parameter Definitions

`params => "(" param (, param)* ")"`

`selfParams => "(" selfParam ("," param)* ")"`

`selfParam => "mut"? "self"`

`param => "mut"? ident ":" ty`

`field => ident ":" ty`

_________________________________
### Argument Definitions

`args => arg ( , arg )*`

`arg => expression`

_________________________________
### Match Definitions

`matchBlock => "{" matchArm ( , matchArm )* ,? "}"`

`matchArm => orPattern "=>" ( blockStmt | expression )`

`orPattern => pattern ( "|" pattern )*`

`pattern => matcher`

`matcher => "_" | string | boolean | number | ( ty identifier? ) `

_________________________________
### Type Definitions

`literal => boolean | "None" | number | string`

`boolean => "true" | "false"`

`ty => basicType | arrayType | traitBoundType | closureType`

`basicType => "bool" | "None" | "u8" | "u16" | "u64" | 
              "i8" | "i16" | "i32" | i64" | "f32" | "f64"`

`arrayType => "[" ty "]"`

`traitBoundType => pathType ( "+" pathType )*`

`pathType => qualifiedIdent generics?`

`closureType => "(" (ty ( , ty )* )? ")" "=>" ( "(" closureType ")" | (basicType | arrayType | pathType) )`


_________________________________
### General Definitions

`path => ident ( "::" ( genericCallsite "::" ident | ident ) )*`

`qualifiedIdent => ident ("::" ident)*`

`fnStmts => "{" fnStmt* "}"`








