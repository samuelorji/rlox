# RLOX
WIP Rust compiler implementation of the `lox` [programming language](https://craftinginterpreters.com/the-lox-language.html) 

## Build
Build the `rlox` binary in release mode:

```bash
cargo build --release
```
binary will be stored in `./target/release/rlox`

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

## Basic Usage

### scoping and simple arithmentic
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
## Compiler Internals