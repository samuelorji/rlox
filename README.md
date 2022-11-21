# RLOX
WIP Rust Interpreter implementation of the `lox` [programming language](https://craftinginterpreters.com/the-lox-language.html) 

## Build
Build the `rlox` binary in release mode:

```bash
cargo build --release
```
binary will be stored in `./target/release/rlox`

## Debug

Run in debug mode to see stack and function chunk OpCodes using the `debug` feature flag:

```bash
cargo run --features debug -- test.lox  
```

Here's a snippet of this function:

```python
fun adder(a,b) {
 return a + b;
}

print adder(10,20);
```

```
== adder ==
0000    2 OP_GET_LOCAL        1
0002    | OP_GET_LOCAL        2
0004    | OP_ADD
0005    | OP_RETURN
0006    3 OP_NIL
0007    | OP_RETURN
=====    =====
== <script> ==
0000    3 OP_CLOSURE          1 <fn adder>
0002    | OP_DEFINE_GLOBAL    0 'adder'
0004    5 OP_GET_GLOBAL       2 'adder'
0006    | OP_CONSTANT         3 '10'
0008    | OP_CONSTANT         4 '20'
0010    | OP_CALL             2
0012    | OP_PRINT
0013    | OP_NIL
0014    | OP_RETURN
=====    =====
          [ <script> ]
0000    3 OP_CLOSURE          1 <fn adder>
          [ <script> ][ <fn adder> ]
0002    | OP_DEFINE_GLOBAL    0 'adder'
          [ <script> ]
0004    5 OP_GET_GLOBAL       2 'adder'
          [ <script> ][ <fn adder> ]
0006    | OP_CONSTANT         3 '10'
          [ <script> ][ <fn adder> ][ 10 ]
0008    | OP_CONSTANT         4 '20'
          [ <script> ][ <fn adder> ][ 10 ][ 20 ]
0010    | OP_CALL             2
          [ <script> ][ <fn adder> ][ 10 ][ 20 ]
0000    2 OP_GET_LOCAL        1
          [ <script> ][ <fn adder> ][ 10 ][ 20 ][ 10 ]
0002    | OP_GET_LOCAL        2
          [ <script> ][ <fn adder> ][ 10 ][ 20 ][ 10 ][ 20 ]
0004    | OP_ADD
          [ <script> ][ <fn adder> ][ 10 ][ 20 ][ 30 ]
0005    | OP_RETURN
          [ <script> ][ 30 ]
0012    | OP_PRINT
30
          [ <script> ]
0013    | OP_NIL
          [ <script> ][ nil ]
0014    | OP_RETURN
```

## Run
`rlox` can be run with a script or in a repl

```bash
rlox test.lox
```
in repl mode:
```bash
rlox
>
```

## Warning:
This rust interpreter aims to use as little of the rusts standard library as possible.

It also uses a lot of pointers / unsafe code, but doesn't leak memory, can be tested with the rust nightly compiler:
```bash
LSAN_OPTIONS=suppressions=lsan.supp RUSTFLAGS="-Z sanitizer=leak" cargo run test.lox
```
As an example, A string is represented by a pointer and a flag to determine if the string is a clone or not (to prevent double free):
```rust
pub struct ObjString {
    length  : usize,
    ptr     : *mut u8,
    hash    : u32,
    isClone : bool
}
```

## Basic Usage

### scoping and simple arithmetic
```python
var a = 2;
{
    var b = a * 5;
    print ("a is " + a);
    print ("b is " + b);
}
```
result:
```bash
a is 2
b is 10
```
### Conditionals
```python
var a = 2;
if (a < 3) {
   print "a is less than 3";
} else {
   print "a is greater than 3";
}
```
result:
```bash
a is less than 3
```

### For Loop
```python
for (var a = 1; a < 5; a = a+1) {
   print ("a is " + a);
}
```
result:
```bash
a is 1
a is 2
a is 3
a is 4
```

### While Loop
```python
var b = 1;
while (b < 5) {
    print ("b is " + b);
    b = b+1;
}
```
result:
```bash
b is 1
b is 2
b is 3
b is 4
```

### Functions
```python
fun adder(a,b) {
 return a + b;
}

fun multiplier (a,b) {
 return a * b ;
}

print adder(3,4) + multiplier(3,4);
```
result:
```bash
19
```

### Function recursion:
```python
fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

var start = clock();
print fib(35);
print clock() - start;
```
result:
```bash
9227465
```

### Closures
```python
fun outer() {
  var a = 1;
  var b = 2;
  fun middle() {
    var c = 3;
    var d = 4;
    fun inner() {
      print a + c + b + d;
    }
    inner();
  }
  middle();
}
outer();
```

result:
```bash
10
```

### Classes 
```python
class Pair {}

var pair = Pair();
pair.first = 1;
pair.second = 2;
print pair.first + pair.second;
```
```bash
3
```

### Classes with Methods
```python
class Foo {
  methodOnFoo() { print "foo"; }
  override() { print "foo"; }
}

class Bar < Foo {
  methodOnBar() { print "bar"; }
  override() { print "bar"; }
}
var bar = Bar();
bar.methodOnFoo(); // expect: foo
bar.methodOnBar(); // expect: bar
bar.override(); // expect: bar
```
result:
```bash
foo
bar
bar
```

### Super Classes
```python
class Base {
  init(a) {
    this.a = a;
  }
}

class Derived < Base {
  init(a, b) {
    super.init(a);
    this.b = b;
  }
}

var derived = Derived("a", "b");
print derived.a; // expect: a
print derived.b; // expect: b
```
result

```bash
a
b
````



