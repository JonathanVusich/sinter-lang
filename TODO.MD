### TODO list
1. Create lookup maps for all `Item` types in each crate. This will allow quick lookup of any `Item` within a crate + quick lookup for any field, generic parameter, or bound method of that `Item`. 
2. Finish implementing `CrateResolver` by utilizing these lookup maps to resolve all functions, fields, generics, etc.