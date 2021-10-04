# flux-lang
 
## Basic syntax

### Package definition and imports
Package specification should be at the top of all source files. 

```
package my.demo

import std::vector
```

### Program entry point
The entry point for a Flux application is the main function.

```
fn main() {
    print("Main entry point...")
}
```

A main argument that takes in `Array<String>` is also valid. The parameters given to the Flux VM at startup are passed to this method.
```
fn main(arguments: Array<String>) {
    print(arguments.to_string())
}
```


