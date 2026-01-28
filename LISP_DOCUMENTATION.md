# 6502 Lisp Parser

A simple Lisp interpreter written entirely in 6502 assembly language. This implementation provides basic S-expression parsing and evaluation of arithmetic and logical operations.

## Architecture

### Memory Layout

```
$0000-$00FF: Zero Page (variables and pointers)
$0100-$01FF: CPU Stack  
$0200-$07FF: Input buffer and workspace
$0800-$0FFF: Symbol table
$1000-$1FFF: Expression evaluation stack
$8000-$FFFF: Program code
```

### Zero Page Variables

```assembly
INPUT_PTR    = $10    ; Pointer to current input position
STACK_PTR    = $12    ; Expression stack pointer  
TEMP         = $14    ; Temporary storage
RESULT       = $16    ; Result accumulator
TOKEN_TYPE   = $18    ; Current token type
TOKEN_VALUE  = $19    ; Current token value (16-bit)
PAREN_COUNT  = $1B    ; Parentheses nesting level
ERROR_FLAG   = $1C    ; Error flag
SYMBOL_LEN   = $1D    ; Length of current symbol
ARGC         = $1E    ; Argument count for functions
```

## Supported Features

### Data Types
- **Numbers**: 16-bit signed integers (0-65535)
- **Symbols**: Variable names and function names
- **Lists**: S-expressions in parentheses

### Built-in Functions

#### Arithmetic Operations
- `(+ a b c ...)` - Addition of multiple numbers
- `(- a b c ...)` - Subtraction (first minus rest)
- `(* a b c ...)` - Multiplication of multiple numbers
- `(/ a b)` - Division (not implemented in basic version)

#### Comparison Operations
- `(= a b)` - Equality test
- `(< a b)` - Less than test
- `(> a b)` - Greater than test

#### Logical Operations
- `(and a b c ...)` - Logical AND of multiple values
- `(or a b c ...)` - Logical OR of multiple values
- `(not a)` - Logical NOT of single value

### Token Types

```assembly
TOK_EOF      = $00    ; End of input
TOK_LPAREN   = $01    ; Left parenthesis '('
TOK_RPAREN   = $02    ; Right parenthesis ')'
TOK_NUMBER   = $03    ; Numeric literal
TOK_SYMBOL   = $04    ; Symbol/identifier
TOK_ERROR    = $FF    ; Parse error
```

## Usage Examples

### Basic Arithmetic
```lisp
(+ 1 2 3)        ; Returns 6
(- 10 3)         ; Returns 7  
(* 2 3 4)        ; Returns 24
```

### Comparisons
```lisp
(= 5 5)          ; Returns 1 (true)
(< 3 5)          ; Returns 1 (true)
(> 10 5)         ; Returns 1 (true)
```

### Logical Operations
```lisp
(and 1 1 1)      ; Returns 1 (true)
(or 0 0 1)       ; Returns 1 (true)
(not 0)          ; Returns 1 (true)
```

### Nested Expressions
```lisp
(+ 1 (* 2 3))    ; Returns 7
(and (= 5 5) (< 3 10))  ; Returns 1 (true)
```

## Implementation Details

### Lexical Analysis
The lexer (`GET_TOKEN`) recognizes:
- Parentheses for list structure
- Numbers (decimal integers)
- Symbols (alphanumeric identifiers)
- Whitespace (ignored)

### Parsing Algorithm
1. **Token-based parsing**: Input is tokenized first
2. **Recursive descent**: Expressions are parsed recursively
3. **Stack-based evaluation**: Uses an expression stack for intermediate results

### Function Call Mechanism
1. Parse opening parenthesis
2. Read function name (symbol)
3. Parse arguments recursively
4. Look up function in symbol table
5. Call function implementation
6. Return result on expression stack

### Error Handling
The parser sets error flags for:
- Unknown tokens/characters
- Malformed expressions
- Unknown functions
- Wrong number of arguments
- Division by zero (when implemented)

## Test Cases

The implementation includes several test cases:

```assembly
TEST_ADD_EXPR:   .byte "(+ 1 2 3)", $00      ; Expected: 6
TEST_SUB_EXPR:   .byte "(- 10 3)", $00       ; Expected: 7  
TEST_MUL_EXPR:   .byte "(* 2 3 4)", $00      ; Expected: 24
TEST_EQ_EXPR:    .byte "(= 5 5)", $00        ; Expected: 1
```

## Files

### lisp_parser.asm
Basic implementation with:
- Core parsing logic
- Simple arithmetic operations
- Basic symbol table

### lisp_extended.asm  
Extended implementation with:
- Full arithmetic operations
- Comparison functions
- Logical operations
- Enhanced error handling
- Comprehensive test suite

## Compilation and Running

To assemble and run (using a 6502 assembler like CA65):

```bash
# Assemble the code
ca65 lisp_parser.asm -o lisp_parser.o

# Link (if using cc65 toolchain)  
ld65 -C none -m map.txt lisp_parser.o -o lisp_parser.bin

# Run in emulator
# (depends on your 6502 emulator/target system)
```

## Limitations

### Current Version
- No floating point arithmetic
- Limited to 16-bit integers
- No user-defined functions
- No variable assignment
- No garbage collection
- No string handling
- No file I/O

### Memory Constraints
- Symbol table has fixed size
- Expression stack has fixed depth
- Input buffer has fixed length
- No dynamic memory allocation

## Potential Enhancements

### Language Features
- Variable definition and assignment
- User-defined functions (lambda)
- String data type and operations
- List manipulation functions (car, cdr, cons)
- Conditional expressions (if, cond)
- Loops and iteration

### System Features
- Garbage collection
- Dynamic memory management
- File I/O operations
- Screen/graphics output
- Keyboard input handling

### Performance
- Optimized multiplication/division
- Better symbol table (hash table)
- Bytecode compilation
- Tail call optimization

## Technical Notes

### 6502-Specific Considerations
- Uses zero page for performance-critical variables
- Leverages 6502 indirect addressing for pointers
- Stack operations use dedicated expression stack (not CPU stack)
- All arithmetic is 16-bit to handle reasonable number ranges

### Assembly Techniques Used
- Indirect addressing through zero page pointers
- Table-driven function dispatch
- Structured programming with subroutines
- Error propagation through status flags

This implementation demonstrates how a simple Lisp interpreter can be built even on an 8-bit processor with limited resources. While not suitable for production use, it serves as an educational example of both Lisp implementation techniques and 6502 assembly programming.