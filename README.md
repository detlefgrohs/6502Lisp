# 6502 Lisp Parser

A complete Lisp interpreter implemented in 6502 assembly language. This project demonstrates how to build a functional programming language interpreter on an 8-bit microprocessor.

## Overview

This project contains a full implementation of a simple Lisp interpreter that can:
- Parse S-expressions
- Evaluate arithmetic operations  
- Perform logical operations
- Handle nested expressions
- Provide meaningful error handling

The interpreter is written entirely in 6502 assembly language and is designed to run on any 6502-based system.

## Features

### Supported Data Types
- **16-bit integers**: Signed values from -32768 to 32767
- **Symbols**: Function names and identifiers
- **Lists**: S-expressions in parentheses

### Built-in Functions

#### Arithmetic
- `(+ a b c ...)` - Addition of multiple numbers
- `(- a b c ...)` - Subtraction (first argument minus the rest)  
- `(* a b c ...)` - Multiplication of multiple numbers

#### Comparison
- `(= a b)` - Test if two numbers are equal
- `(< a b)` - Test if first number is less than second
- `(> a b)` - Test if first number is greater than second

#### Logical
- `(and a b c ...)` - Logical AND of multiple values
- `(or a b c ...)` - Logical OR of multiple values  
- `(not a)` - Logical NOT of a single value

### Example Expressions
```lisp
(+ 1 2 3)                    ; Returns 6
(- 20 5 3)                   ; Returns 12
(* 2 3 4)                    ; Returns 24
(= 10 10)                    ; Returns 1 (true)
(< 5 10)                     ; Returns 1 (true)  
(and (= 5 5) (< 3 10))       ; Returns 1 (true)
(+ (* 2 3) (- 10 5))         ; Returns 11
```

## Files

### Core Implementation
- **`lisp_parser.asm`** - Basic Lisp parser with core functionality
- **`lisp_extended.asm`** - Extended version with full feature set
- **`examples.asm`** - Test cases and example expressions

### Documentation  
- **`LISP_DOCUMENTATION.md`** - Detailed technical documentation
- **`README.md`** - This file

## Memory Layout

```
Address Range    | Usage
----------------|------------------------------------------
$0000-$00FF     | Zero page variables and pointers
$0100-$01FF     | 6502 CPU stack
$0200-$07FF     | Input buffer and workspace  
$0800-$0FFF     | Symbol table for built-in functions
$1000-$1FFF     | Expression evaluation stack
$8000-$FFFF     | Program code
```

## Building and Running

### Requirements
- 6502 assembler (such as CA65 from CC65 package)
- 6502 emulator or target hardware

### Assembly
```bash
# Using CA65 assembler
ca65 lisp_parser.asm -o lisp_parser.o
ca65 lisp_extended.asm -o lisp_extended.o  
ca65 examples.asm -o examples.o

# Link (configuration depends on your target)
ld65 -C none lisp_parser.o -o lisp_parser.bin
```

### Running
Load the assembled binary into your 6502 emulator or hardware at address $8000 and execute.

## Architecture

### Parsing Strategy
The parser uses a **recursive descent** approach:

1. **Lexical Analysis**: Input text is tokenized into meaningful units
2. **Syntactic Analysis**: Tokens are parsed into expression trees
3. **Evaluation**: Expressions are evaluated using a stack-based approach

### Token Types
```assembly
TOK_EOF      = $00    ; End of input
TOK_LPAREN   = $01    ; Left parenthesis '('  
TOK_RPAREN   = $02    ; Right parenthesis ')'
TOK_NUMBER   = $03    ; Numeric literal
TOK_SYMBOL   = $04    ; Symbol/identifier
TOK_ERROR    = $FF    ; Parse error
```

### Function Dispatch
Built-in functions are stored in a symbol table and dispatched through function IDs:

```assembly
FUNC_ADD     = $01    ; Addition
FUNC_SUB     = $02    ; Subtraction  
FUNC_MUL     = $03    ; Multiplication
FUNC_EQ      = $05    ; Equality test
FUNC_LT      = $06    ; Less than test
; ... etc
```

## Performance Characteristics

### Memory Usage
- **Code size**: ~2KB for basic version, ~4KB for extended
- **RAM usage**: ~2KB for buffers and workspace
- **Stack depth**: Limited by available RAM (typically 50+ nested calls)

### Execution Speed  
- **Simple expression**: ~1000 cycles
- **Complex nested**: ~5000+ cycles
- **Performance**: Suitable for interactive use on 1MHz 6502

## Error Handling

The parser provides comprehensive error detection:

```assembly
ERROR_FLAG values:
$01 - Unexpected token
$02 - Malformed list expression  
$03 - Unknown function
$04 - Wrong number of arguments
$05 - Division by zero (when implemented)
$06 - Comparison error
$07 - Less than comparison error
$08 - Greater than comparison error
$09 - Logical operation error
```

## Testing

The implementation includes a comprehensive test suite with:

- **Basic arithmetic tests**: Addition, subtraction, multiplication
- **Comparison tests**: Equality, less than, greater than  
- **Logical operation tests**: AND, OR, NOT
- **Nested expression tests**: Complex multi-level expressions
- **Error condition tests**: Invalid syntax and unknown functions

Run tests by calling `RUN_ALL_EXAMPLES` after assembly.

## Limitations

### Current Implementation
- **No floating point**: Only integer arithmetic
- **Fixed precision**: 16-bit signed integers only
- **No user functions**: Cannot define custom functions
- **No variables**: No assignment or variable storage
- **No I/O**: No file or device input/output
- **No strings**: Only numbers and symbols

### Memory Constraints
- **Fixed buffers**: Input buffer and stacks have fixed sizes
- **No garbage collection**: Memory is statically allocated  
- **Limited symbols**: Symbol table has fixed capacity

## Potential Enhancements

### Language Features
```lisp
; Variable assignment
(define x 10)
(set! x 20)

; User-defined functions  
(define (square x) (* x x))

; Conditional expressions
(if (> x 0) x (- x))

; String operations
(concat "Hello " "World")

; List manipulation
(car '(1 2 3))        ; Returns 1
(cdr '(1 2 3))        ; Returns (2 3)  
(cons 1 '(2 3))       ; Returns (1 2 3)
```

### System Enhancements
- **Dynamic memory allocation**
- **Garbage collection**  
- **File I/O operations**
- **Graphics and sound output**
- **Interrupt-driven input**

## Educational Value

This project demonstrates several important concepts:

### Programming Language Implementation
- **Lexical analysis** and tokenization
- **Recursive descent parsing** 
- **Abstract syntax trees**
- **Stack-based evaluation**
- **Symbol table management**

### 6502 Assembly Techniques
- **Indirect addressing** for dynamic data structures
- **Zero page optimization** for frequently used variables
- **Structured programming** with subroutines
- **Table-driven dispatch** for function calls
- **Error propagation** in assembly language

### Computer Science Fundamentals  
- **Formal language theory**
- **Compiler/interpreter design**
- **Data structure implementation** 
- **Algorithm optimization** for limited resources

## Historical Context

This implementation pays homage to the early days of personal computing when:
- **Memory was precious** (measured in kilobytes)
- **CPU speed was limited** (1-2 MHz)
- **Languages were often implemented in assembly** for performance
- **Educational computers** like the Apple II popularized programming

The 6502 processor powered many iconic systems:
- **Apple II series** (1977-1993)
- **Commodore 64** (1982-1994)  
- **Nintendo Entertainment System** (1985-1995)
- **BBC Micro** (1981-1994)

## Contributing

This is an educational project, but contributions are welcome:

- **Bug fixes** in the parser logic
- **Additional built-in functions**
- **Performance optimizations**
- **Documentation improvements**
- **Example programs**

## License

This project is released into the public domain for educational use.

## References

- *The Structure and Interpretation of Computer Programs* - Abelson & Sussman
- *6502 Assembly Language Programming* - Lance Leventhal  
- *Programming the 6502* - Rodnay Zaks
- *Lisp in Small Pieces* - Christian Queinnec