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

## Compiler Internals