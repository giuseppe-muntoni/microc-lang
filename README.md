# MicroC Compiler

MicroC is a subset of the language C with the following simplification:

* It supports only integers (`int`), characters (`char`), booleans (`bool`) and floats as scalar values, array and pointers as compound data types;
* No function pointers;
* No dynamic allocation of memory;
* No shorthand for initialize variable during declaration;
* Functions can only return `void`, `int`, `bool`, `char`;
* It has separate compilation but without preprocessor, include guards, and header files. Only simple declaration of external functions or variables in source files.
* the runtime support is minimal and contains simple function to print and to read.
* No overloading

## MicroC Tasks
- [x] **Parsing**: Implementation of the scanner using Ocamllex and the lr(1) parser using Menhir
- [x] **Semantic analysis**: implementation of a static analysis for check that a given program obeys the scoping rule and the type system of the language.
- [x] **Code generation**: usage of the LLVM toolchain to compile a MicroC program to low level code (LLVM bitcode) and to perform some simple optimizations.

**Extensions**:
Here there are some extensions that I want to add to the language in its simplest version:
- [ ] pre/post increment/decrement operators, i.e., `++` and `--`, and  abbreviation for assignment operators, i.e., `+=`, `-=`, `*=`, `/=` and `%=`;
- [ ] add structures and unions as compound data type
- [ ] pointers arithmetic and multi-dimensional arrays as in C;
- [x] floating point arithmetic and strings as in C, i.e. null-terminated arrays of characters;
- [x] a new semantic analysis pass to detect deadcode;
- [x] seperate compilation.

The pdf microc-lang_report contains a simple formalization of the type system and the static analysis of the language.
