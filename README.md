# The Flux programming language

Flux is a statically-typed and garbage collected language that is designed around user ergonomics and productivity.
It features advanced garbage collection, message-based concurrency and generic types to allow users to develop systems
quickly, scalably, and correctly.

## Basic syntax

### Imports

```ignorelang
import std::vector
```
Imports should be declared at the top of each source file. 

### Program entry point
The entry point for a Flux application is the main function.

```ignorelang
fn main() {
    print("Main entry point...");
}
```

A main argument that takes in a string array is also valid. The parameters given to the Flux VM at startup are passed to this method.
```ignorelang
fn main(arguments: Array<String>) {
    print(arguments.to_string());
}
```

### Functions
An example function with two signed 32-bit integer parameters and a signed 64-bit integer return type.

```ignorelang
fn sum(a: i32, b: i32) -> i64 {
    return (a as i64) + (b as i64);
}
```

A function that returns no meaningful value.
```ignorelang
fn print_sum(a: i32, b: i32) {
    print("sum of {a} and {b} is {a + b}.");
}
```

### Variables
Read-only local variables are defined using the keyword `val`. They can be assigned a value once.

```ignorelang
val a: i32 = 1; // Immediate assignment
val b = 2; // `i32` type is inferred
val c: i32; // Type is required when no initializer is provided
c = 3; // Deferred assignment
```

Variables that can be reassigned use the `var` keyword.

```ignorelang
var x = 5; // `i32` type is inferred
x += 1;
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


## Bytecode reference

### Opcodes
```ignorelang
OpCode::ReturnVoid
```
This opcode is used to terminate execution of a function with no return value.

```ignorelang
OpCode::Return32
```
This opcode is used to terminate execution of a function and return a 32-bit value to the callee.

```ignorelang
OpCode::Return64
```
This opcode is used to terminate execution of a function and return a 64-bit value to the callee.

```ignorelang
OpCode::Pop32
```
This opcode is used to pop a 32-bit value off of the stack and discard it.

```ignorelang
OpCode::Pop64
```
This opcode is used to pop a 64-bit value off of the stack and discard it.

```ignorelang
OpCode::GetConstant32
```
This opcode is used to push a 32-bit global variable onto the stack. It must be followed by a 24-bit index that 
corresponds to the index into the constant pool.

```ignorelang
OpCode::GetConstant64
```
This opcode is used to push a 64-bit global variable onto the stack. It must be followed by a 24-bit index that
corresponds to the index into the constant pool.

```ignorelang
OpCode::SetLocal32
```
This opcode is used to set a 32-bit local variable from the value currently on the stack. It must be followed by a
byte-sized index that corresponds to the index into the local variable storage in the stackframe.

```ignorelang
OpCode::SetLocal64
```
This opcode is used to set a 64-bit local variable from the value currently on the stack. It must be followed by a
byte-sized index that corresponds to the index into the local variable storage in the stackframe.

```ignorelang
OpCode::GetLocal32
```
This opcode is used to push a 32-bit local variable onto the stack. It must be followed by a
byte-sized index that corresponds to the index into the local variable storage in the stackframe. 

```ignorelang
OpCode::GetLocal64
```
This opcode is used to push a 64-bit local variable onto the stack. It must be followed by a
byte-sized index that corresponds to the index into the local variable storage in the stackframe.

```ignorelang
OpCode::Jump
```
This opcode is used to jump to the given offset within the executable code. It must be followed by a 24-bit offset
that corresponds to the byte index to jump to.

```ignorelang
OpCode::JumpBack
```
This opcode is used to jump to the given offset within the executable code. It must be followed by a 24-bit offset
that corresponds to the byte index to jump to.

```ignorelang
OpCode::Call
```
This opcode is used to call a procedure. It must be followed by a 24-bit offset specifying the amount of stack space to 
allocate to the stackframe followed by a 32-bit offset to jump to.
