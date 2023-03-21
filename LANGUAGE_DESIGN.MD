### AST design

All AST nodes have a `u32` unique key that represents the index of that node into a vector of AST nodes.
This allows fast and efficient lookups by the AST visitors to resolve nodes by id.
AST nodes also have an internal `Span` struct that maps the source position and the end position of the 
AST node within its source file. This is useful for printing helpful error messages and knowing where
to highlight nodes within a source code editor.


