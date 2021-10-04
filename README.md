# flux-lang
 
## Basic syntax

### Package definition and imports
Package specification should be at the top of all source files. 

```ignorelang
package my.demo

import std::vector
```

### Program entry point
The entry point for a Flux application is the main function.

```ignorelang
fn main() {
    print("Main entry point...")
}
```

A main argument that takes in a string array is also valid. The parameters given to the Flux VM at startup are passed to this method.
```ignorelang
fn main(arguments: Array<String>) {
    print(arguments.to_string())
}
```

### Functions
An example function with two signed 32-bit integer parameters and a signed 64-bit integer return type.

```ignorelang
fn sum(a: i32, b: i32) -> i64 {
    return (a as i64) + (b as i64)
}
```

A function body can be an expression. Its return type is inferred as `i32`.

```ignorelang
fn sum(a: i32, b: i32) = a + b
```

A function that returns no meaningful value.
```ignorelang
fn print_sum(a: i32, b: i32) {
    print("sum of {a} and {b} is {a + b}.")
}
```

### Variables
Read-only local variables are defined using the keyword `val`. They can be assigned a value once.

```ignorelang
val a: i32 = 1 // Immediate assignment
val b = 2 // `i32` type is inferred
val c: i32 // Type is required when no initializer is provided
c = 3 // Deferred assignment
```

Variables that can be reassigned use the `var` keyword.

```ignorelang
var x = 5 // `i32` type is inferred
x += 1
```

Variables can be defined within a class or function definition. 

### Creating classes and instances

To define a class, use the `class` keyword.

```ignorelang
class Shape
```

Properties of a class can be listed in 
