# flux-lang

## Technical specification

### Struct
The max size of a struct must be able to fit into 24 bits. This enables pop and push instructions to only take up 32
bits which allows them to fill only a single register on a 32 bit machine.

## Bytecode reference

### Opcodes
```ignorelang
OpCode::Return 
```
This opcode is used to terminate execution of a function. The VM will track the state of the current function call to 
know the size of the return type.

```ignorelang
OpCode::Pop
```
This opcode is used to pop a value off of the stack. It is followed by a 24 bit offset to define the number of bytes to
pop off the stack.

```ignorelang
OpCode::Push
```
This opcode is used to push a value onto the stack. It is followed by a 24 bit offset to define the number of bytes to
push onto the stack.


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

Properties of a class can be listed in its declaration.

```ignorelang
class Rectangle(width: f64, length: f64) {
    fn perimeter() {
        return (height + length) * 2
    }
}
```

The default constructor with the class parameters is available automatically.

```ignorelang
val rectangle = new Rectangle(5.0, 2.0)
println("The perimeter is {rectangle.perimeter()}")
```

Inheritance between classes is declared by a colon (`:`). Classes are final by default; to make a class extendable, mark it as `open`.

```ignorelang
open class Shape

class Rectangle(width: f64, length: f64): Shape()
```

### Comments
Like most languages, Flux supports single-line (or end-of-line) and multi-line (block) comments.

```ignorelang
// This is an end-of-line comment

/* This is a block comment
   on multiple lines. */
```