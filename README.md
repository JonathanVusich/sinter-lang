# The Sinter programming language

Sinter is a statically-typed and garbage collected language that is designed around user ergonomics and productivity.
It features advanced garbage collection, message-based concurrency and generic types to allow users to develop systems
quickly, scalably, and correctly.

## Basic syntax

### Use statements 

```ignorelang
use std::vector::Vector;
```
Imports must be declared at the top of each source file. 

### Program entry point
The entry point for a Sinter application is the main function.

```ignorelang
fn main() {
    print("Main entry point...");
}
```

A main argument that takes in a string array is also valid. 
The parameters given to the Sinter VM at startup are passed to this method.
```ignorelang
fn main(arguments: [str]) {
    print(arguments.to_string());
}
```

### Built-in Types

Sinter provides a number of built-in types in order to simplify application development.

#### Unsigned integer types
`u8`, `u16`, `u32`, `u64`

#### Signed integer types
`i8`, `i16`, `i32`, `i64`

#### Floating point types
`f32`, `f64`

#### Object types
The `str` type is an internal class that contains an immutable array of bytes that represent a UTF-8 encoded string.
This type can be created through a literal.

```ignorelang
let greeting = "Hello world!"; // `str` type is inferred
```

The `[]` type is a generic array type that can contain both integer and floating point types
in addition to user defined types.

```ignorelang
class Point {
    x: f64, 
    y: f64
};
ref class Node;

let i32_array: [i32] = [1, 2, 3];
let point_array = [Point { 1.0, 2.0 }, Point { 1.5, 2.5 }];
let node_array = [Node { }, Node { }];
```

### Functions
An example function that returns the sum of two integers.

```ignorelang
fn sum(a: i64, b: i64) => i64 {
    a + b
}
```

### Variables
Local and global variables can be assigned using the keyword `let`.

```ignorelang
let a: i64 = 1; // Immediate assignment
let b = 2; // `i64` type is inferred
```

Variables that can be reassigned must be prefixed with the `mut` keyword. Global variables cannot be mutable.

```ignorelang
fn generate_num() => i64 {
    let mut x = 5; // `i64` type is inferred
    x + 1
}
```

Variables can be defined within a struct or function definition.

### Functions

To define a  function, use the `fn` keyword.

```ignorelang
fn add(a: i64, b: i64) => i64 {
    a + b
} 
```

Functions that do not return a value can omit the return value in the 
function signature.

```ignorelang
fn print(text: str) {
    println(text);
}
```

Functions may return alternative values such as an error or empty value by using the `|` operator to denote a union type.

```ignorelang
fn write<T: Serializable>(value: T) => None | WriteError {
    let bytes = value.to_bytes();
    if (buffer.remaining() < bytes.len()) {
        WriteError::BufferOverflow
    } else if (!buffer.write_bytes(bytes)) {
        WriteError::MalformedBytes
    } else {
        None
    }
}

enum WriteError( 
    BufferOverflow,
    MalformedBytes,
);
```

Functions can operate on types that implement traits through either the use of generic types
or through a direct reference to the trait itself.

```ignorelang
trait Loggable {
    fn format_log_message(self) => str;
}

fn log_specialized_message<T: Loggable + Serializable>(message: T) {
    println(message.format_log_message());
}

fn log_trait_message(message: Loggable + Serializable) {
    println(message.format_log_message());
}

```

Classes can also declare generic types as members. Using a generic type allows the compiler to generate  
class definitions for each concrete type. This improves performance significantly at the expense of compile time overhead
and binary size. 

Using a trait reference only generates a single class definition, but has performance limitations. Reference classes
already contain extra information for introspection, but inline classes and primitives have to be wrapped in a fat 
pointer in order for the runtime to inspect the objects at runtime. This uses more processor cycles and memory, 
but can be a valuable tool in cold paths where there are many different types being passed.

```ignorelang
ref class List<T> {
    array: [T],
    
    pub fn new(initial_capacity: u64) -> Self {
        Self([T; initial_capacity])
    }
    
    pub fn push(mut self, item: T) {
        ...
    }
}

let list_of_ints = List::<i64>::new();
let list_of_lists = List::<List<f64>>::new();
let trait_bounds = List::<Loggable + Serializable>::new();
let unions = List::<str | i64 | f64>::new();
```

### Defining classes and instances

To define a class, use the `class` keyword.

```ignorelang
ref class Shape;
```

Properties of a class are listed in its declaration.

```ignorelang
class Rectangle {
    width: f64, 
    length: f64
}
```

Classes can be constructed by calling the qualified path with values for each class field.
```ignorelang
let rectangle = Rectangle { 10, 20 };
```

Classes can contain instance and static method declarations. Instance methods are marked by providing a `self`
argument in the declaration.
```ignorelang
class Rectangle {
    width: f64;
    length: f64;
    
    fn perimeter(self) => f64 {
        return (self.width * 2) + (self.height * 2);
    }
    
    fn square(size: f64) => Self {
        return Self(size, size);
    }
}
```

Instance fields can only be mutated from instance methods that take `mut self` instead of `self`.
#### Incorrect:
```ignorelang
class Counter { 
    num: i64;
    
    fn increment(self) {
                 ^^^^  Error: self must be marked as mutable in order to modify the num field.
        self.num = self.num + 1;
    }
}
```
#### Correct:
```ignorelang
class Counter { 
    num: i64,
    
    fn increment(mut self) {
                 ^^^^  Error: self must be marked as mutable in order to modify the num field.
        self.num = self.num + 1;
    }
}
```

Likewise, classes cannot be mutated if the mutator does not have a mutable reference to the instance.
This is done in order to ensure strong immutability by default.
#### Incorrect:
```ignorelang
let counter = Counter { 0 };

counter.increment();
       ^^^^^^^^^^^^  Error: Attempted to call a mutable method on an immutable variable.
```
#### Correct:
```ignorelang
let mut counter = Counter { 0 };

counter.increment();
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
};
```

Enums can also contain a payload and functions that are specific to each member:
```ignorelang
enum Message {
    Text(message: str),
    Photo(caption: str, photo: SerializedPhoto) {
        fn size(self) => u64 {
            return self.photo.size();
        }
    },
};
```

Enums can also contain functions that are not specific to each member:
```ignorelang
enum Planet {
    Mercury,
    ...,
    
    fn diameter(self) => f64 {
        match self {
            Mercury => ...,
            ...
        }
    }
};
```

### Traits

Traits are used to describe behavior about a type in a way that allows
them to be used in a more generic way.

They can only contain function declarations or function implementation.

```ignorelang
trait StringIterator {
    fn next(self) => str | None;
}
```

Traits can also use generic parameters in order to improve usability.

```ignorelang
trait Iterator<T> {
    fn next(self) => T | None;
}

ref class MutableList<T> {
    array: [T],
    
    fn extend<I: Iterator<T>>(mut self, iterator: I) {
        while true {
            match iterator.next() {
                T item => self.add(item),
                None => break;
            }
        }
    }
}
```
### Pattern matching

Sinter provides a `match` expression which is very useful for dispatching complicated control flow.

```ignorelang
match number {
    1 => print("One!"),
    2 | 3 | 5 | 7 | 11 => print("This is a prime"),
    _ => print("Unremarkable"),
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

ref class SortedMap<T: Sortable + Hashable>;
```

Trait bounds can also be used to directly describe mixed types. This will incur a performance
penalty for non-reference types since they will need to be wrapped in a reference pointer in order
to be mixed with other types.


```ignorelang
trait Node {
    fn bounds(self) => Bounds;
    fn draw(self, graphics: mut Graphics);
    fn children(mut self) => MutableList<Node>;
}

fn draw_frame(nodes: List<Node>) {
    ...
}
```

### Concurrency

Sinter helps users to write correct concurrent programs by preventing concurrent
references to types that do not implement the `std::Sync` trait. 

It is entirely possible to misuse the `std::Sync` trait and create deadlocks or other concurrency bugs. 
This trait is a special empty trait that allows the compiler to check thread boundary access at compile time, and to prevent **accidental**
concurrent usage of types that do not implement `std::Sync`.

The primitive types in Sinter all implement `std::Sync` since they are immutable.

Closures can be sent across thread boundaries if all of their captured variables implement `std::Sync`.

```ignorelang
use std::Sync;

ref class ConcurrentMap<K, V>(...) { ... };

impl Sync for ConcurrentMap<K, V>;
```

### Comments
Like most languages, Sinter supports single-line (or end-of-line) and multi-line (block) comments.

```ignorelang
// This is an single/end-of-line comment

/* This is a block comment
   on multiple lines. */
```
