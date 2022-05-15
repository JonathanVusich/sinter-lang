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
fn main(arguments: [str]) {
    print(arguments.to_string());
}
```

### Built-in Types

Flux provides a number of built-in types in order to simplify application development.


#### Unsigned integer types
`u8`, `u16`, `u32`, `u64`

#### Signed integer types
`i8`, `i16`, `i32`, `i64`

#### Floating point types
`f32`, `f64`

#### Object types
The `str` type is an internal class that contains an immutable array of bytes that represent a UTF-8 encoded string.
This type can be created through a literal or from an array of bytes.

```ignorelang
val greeting = "Hello world!"; // `str` type is inferred

val bytearray: [u8] = [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33];
val greeting_from_array = str(bytearray); // "Hello world!"
```


The `[]` type is a generic array type that can contain both integer and floating point types
in addition to inline classes and references.



### Functions
An example function that returns the sum of two integers.

```ignorelang
fn sum(a: i64, b: i64) -> i64 {
    return a + b;
}
```

A function that returns no meaningful value.
```ignorelang
fn print_sum(a: i64, b: i64) {
    print("sum of {a} and {b} is {a + b}.");
}
```

### Variables
Read-only local variables are defined using the keyword `val`. They can be assigned a value once.

```ignorelang
val a: i64 = 1; // Immediate assignment
val b = 2; // `i64` type is inferred
val c: i64; // Type is required when no initializer is provided
c = 3; // Deferred assignment
```

Variables that can be reassigned use the `var` keyword.

```ignorelang
var x = 5; // `i64` type is inferred
x += 1;
```

Variables can be defined within a struct or function definition.

### Functions

To define a  function, use the `fn` keyword.

```ignorelang
fn add(a: i64, b: i64) -> i64 {
    return a + b;
} 
```

Functions that do not return a value can omit the return value in the 
function signature.

```ignorelang
fn print(text: str) {
    println(text);
}
```

Functions may return an alternate type such as an error or empty value should use the `|` operator
to separate the different types.

```ignorelang
fn find_user(user_name: str) -> User | None {
    ...
}

fn load_user_info(user: User) -> UserInfo | LoadError {
    ...
}

enum LoadError {
    Timeout,
    ConnectionClosed,
}
```



Functions that may return an error . It is recommended to use an `enum` to return values
where various error conditions may occur. 

```ignorelang
enum ReadResult {
    Result(contents: str),
    FileNotFound,
    FileLockInterrupted,
}
```

### Defining classes and instances

To define a class, use the `class` keyword.

```ignorelang
class Shape
```

Properties of a class are listed in its declaration.

```ignorelang
class Rectangle(width: f64, length: f64) {
    fn perimeter() {
        return (height + length) * 2
    }
}
```

The default constructor with the class parameters is available automatically.

```ignorelang
val rectangle = Rectangle(5.0, 2.0)
println("The perimeter is {rectangle.perimeter()}")
```

### Defining enums

To define an enum, use the `enum` keyword.

```ignorelang
enum Planet {
    Mercury,
    Venus,
    Earth,
    Mars,
    Jupiter,
    Saturn,
    Uranus,
    Neptune,
}
```

Enums can also contain a payload and functions that are specific to each member.

```ignorelang
enum Message {
    Text(message: str),
    Photo(caption: str, photo: SerializedPhoto) {
        fn size() {
            return photo.size()
        }
    },
}
```

### Traits

Traits are used to describe behavior about a type in a way that allows
them to be used in a more generic way.

They can only contain function declarations or function implementation.

```ignorelang
trait Describable {
    fn name() -> str;
    fn description() -> str;

    fn describe() -> str {
        return "Name: {name()}, Description: {description()}";
    }
}
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
