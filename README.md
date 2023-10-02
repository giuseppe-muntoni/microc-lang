# MicroC Compiler

MicroC is a subset of the language C with the following simplification:

* It supports only integers (`int`), characters (`char`) and booleans (`bool`) as scalar values, array and pointers as compound data types;
* There are no structures, unions, doubles, function pointer;
* No dynamic allocation of memory;
* No multi-dimensional arrays;
* No shorthand for initialize variable during declaration;
* Functions can only return `void`, `int`, `bool`, `char`;
* No pointer arithmetic;
* Pointers and arrays are not interchangeable;
* no separate compilation, all the code of a program must stay in a unique compilation unit;
* there are only two library functions
```C
void print(int)  // it outputs an integer to standard output
int getint()     // it inputs an integer from standard input 
```

## MicroC Tasks
- [x] **Parsing**: Implementation of the scanner using Ocamllex and the lr(1) parser using Menhir
- [ ] **Semantic analysis**: implementation of a static analysis for check that a given program obeys the scoping rule and the type system of the language.
- [ ] **Code generation**: usage of the LLVM toolchain to compile a MicroC program to low level code (LLVM bitcode) and to perform some simple optimizations.

**Extensions**:
- [ ] pre/post increment/decrement operators, i.e., `++` and `--`, and  abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
- [ ] pointers, arrays & multi-dimensional arrays as in C;
- [ ] floating point arithmetic and strings as in C, i.e. null-terminated arrays of characters;
- [ ] a new semantic analysis pass to detect deadcode;
- [ ] seperate compilation.
