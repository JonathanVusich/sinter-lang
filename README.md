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
fn sum(a: i64, b: i64) => i64 {
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
fn add(a: i64, b: i64) => i64 {
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
fn find_user(user_name: str) => User | None {
    ...
}

fn load_user_info(user: User) => UserInfo | None | LoadError {
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

Functions can operate on types that implement traits through either the use of generic types
or through a direct reference to the trait itself.

```ignorelang
trait Loggable {
    fn format_log_message() => str;
}

fn send_message<T: Serializable>(message: T, server: Server) {
    ...
}

fn log_message<T: Loggable + Serializable>(message: T) {
    println(message.format_log_message());
    send_message(message, logging_server);
}

fn log_message(message: Loggable + Serializable) {
    println(message.format_log_message());
    send_message(message, logging_server);
}
```

Using a generic type allows specialized code to be generated for each
concrete type which improves performance at the expense of compile time
code generation.

Using a trait reference directly will force all types to be pointer-sized,
which may incur a boxing penalty.

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
            return photo.size();
        }
    },
}
```

### Traits

Traits are used to describe behavior about a type in a way that allows
them to be used in a more generic way.

They can only contain function declarations or function implementation.

```ignorelang
trait StringIterator {
    fn next() => str | None;
}
```

Traits can also use generic parameters in order to improve usability.

```ignorelang
trait Iterator<T> {
    fn next() => T | None;
}

class MutableList<T> {
    ...
    
    fn extend<I: Iterator<T>>(iterator: I) {
        while true {
            match iterator.next() {
                T item => self.add(item),
                None => break;
            }
        }
    }
}
```

### Generic types

Functions and class can use generic type definitions in order to allow usability with
many concrete types. 

```ignorelang
fn maybe<T>(instance: T) => T | None {
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

trait Hashable {
    ...
}

fn sort<T: Sortable>(list: MutableList<T>) {
    ...
}

class SortedMap<T: Sortable + Hashable>;
```

Trait bounds can also be used to directly describe mixed types. This will incur a performance
penalty for non-reference types since they will need to be wrapped in a reference pointer in order
to be mixed with other types.


```ignorelang
trait Node {
    fn bounds() => Bounds;
    fn draw(Graphics g);
    fn children() => MutableList<Node>;
}

fn draw_frame(nodes: List<Node>) {
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