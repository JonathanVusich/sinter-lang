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
in addition to inline classes, enums and references.

```ignorelang
inline class Point(x: f64, y: f64);
class Node;

val i32_array: [i32] = [1, 2, 3];
val point_array = [Point(1.0, 2.0), Point(1.5, 2.5)];
val node_array = [Node(), Node()];
```

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

Functions may return alternate types such as an error or empty value by using the `|` operator.

```ignorelang
fn find_user(user_name: str) -> User | None {
    ...
}

fn load_user_info(user: User) -> UserInfo | None | LoadError {
    ...
}

match load_user_info(...) {
    UserInfo info => {...},
    LoadError error => {...},
    None => {...},
}

enum LoadError {
    Timeout,
    ConnectionClosed,
}
```

### Defining classes and instances

To define a class, use the `class` keyword.

```ignorelang
class Shape;
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
val rectangle = Rectangle(5.0, 2.0);
println("The perimeter is {rectangle.perimeter()}");
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

### Generic types

Functions and class can use generic type definitions in order to allow usability with
many concrete types. 

```ignorelang
fn maybe<T>(instance: T) -> T | None {
    ...
}

class Point<T>(first: T, second: T);
class Point<T, U>(first: T, second: U);
```

Generic type definitions can also be restricted by trait bounds.

```ignorelang
trait Sortable {
    ...
}

fn sort<T: Sortable>(list: MutableList<T>) {
    ...
}
```

### Comments
Like most languages, Flux supports single-line (or end-of-line) and multi-line (block) comments.

```ignorelang
// This is an end-of-line comment

/* This is a block comment
   on multiple lines. */
```