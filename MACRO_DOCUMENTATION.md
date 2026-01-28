# Macro System Documentation

The 6502 Lisp parser now includes a comprehensive macro system that allows defining reusable code patterns that expand at parse time. This document explains the macro features and how to use them.

## Overview

Macros are a powerful feature of Lisp that allow you to define new syntactic constructs. They are functions that take code as input and return transformed code as output. The macro expansion happens at parse time, before evaluation.

## Macro Definition

### Basic Syntax
```lisp
(defmacro name (parameters) body)
```

### Examples

#### Simple Replacement Macro
```lisp
(defmacro nil () 0)
```
Usage: `nil` → Expands to: `0`

#### Conditional Macros
```lisp
; WHEN macro - execute body if condition is true
(defmacro when (condition body)
  (list 'if condition body))
```
Usage: `(when (> x 0) (print x))` → Expands to: `(if (> x 0) (print x))`

```lisp
; UNLESS macro - execute body if condition is false  
(defmacro unless (condition body)
  (list 'if (list 'not condition) body))
```
Usage: `(unless (= x 0) (/ 10 x))` → Expands to: `(if (not (= x 0)) (/ 10 x))`

#### Utility Macros
```lisp
; INC macro - increment a value
(defmacro inc (x) (list '+ x 1))
```
Usage: `(inc 5)` → Expands to: `(+ 5 1)` → Evaluates to: `6`

```lisp
; SQUARE macro - square a number
(defmacro square (x) (list '* x x))
```
Usage: `(square 4)` → Expands to: `(* 4 4)` → Evaluates to: `16`

## Built-in Macros

The system comes with several predefined macros:

### WHEN
Conditional execution when condition is true.
```lisp
(when condition body)
```
Expands to: `(if condition body)`

### UNLESS  
Conditional execution when condition is false.
```lisp
(unless condition body)
```
Expands to: `(if (not condition) body)`

## Memory Layout

### Macro Table Structure
```
Address Range: $1400-$17FF (1KB)
Entry Size: 64 bytes per macro
Maximum Macros: 16

Entry Format:
Offset 0-15:  Macro name (null-terminated)
Offset 16:    Parameter count
Offset 17-63: Macro body template
```

### Zero Page Variables
```assembly
MACRO_PTR    = $20    ; Pointer to current macro entry
MACRO_ARGC   = $22    ; Macro argument count  
EXPAND_FLAG  = $23    ; Macro expansion in progress
MACRO_DEPTH  = $24    ; Recursion depth (max 5)
```

## Implementation Details

### Macro Lookup Process
1. When a symbol is encountered, first check if it's a macro
2. If macro found, initiate expansion process
3. If not a macro, check built-in function table
4. If neither, treat as unknown symbol

### Expansion Process
1. Validate argument count matches macro parameters
2. Check recursion depth (prevent infinite expansion)
3. Substitute parameters in macro body template
4. Parse expanded text as new expression
5. Evaluate resulting expression

### Parameter Substitution
The macro body uses special placeholders:
- `$01` - First parameter
- `$02` - Second parameter  
- `$03` - Third parameter
- etc.

Example template: `"(if " $01 " " $02 ")"` becomes `"(if (> x 0) (print x))"`

## Error Handling

### Error Codes
```assembly
$0A - DEFMACRO syntax error
$0B - QUOTE function error
$0C - UNQUOTE function error  
$0D - Macro recursion too deep
$0E - Wrong number of macro arguments
```

### Recursion Prevention
- Maximum recursion depth: 5 levels
- Prevents infinite macro expansion
- Error generated if depth exceeded

## Advanced Features

### Quote and Unquote
```lisp
; QUOTE - prevent evaluation
(quote expression)  ; or 'expression
```

```lisp
; UNQUOTE - force evaluation within quote
(unquote expression)  ; or ,expression
```

### Variable Argument Macros
```lisp
; PROGN - execute multiple expressions
(defmacro progn (&rest body)
  (cons 'begin body))
```

### Macro Predicates
```assembly
; Check if symbol is a macro
IS_MACRO:
    JSR LOOKUP_MACRO
    ; Returns non-zero if macro found
```

## Testing Framework

### Test Cases
The implementation includes comprehensive tests:

1. **WHEN macro test**: `(when 1 42)` → Should return 42
2. **UNLESS macro test**: `(unless 0 42)` → Should return 42  
3. **INC macro test**: `(inc 5)` → Should return 6
4. **DEC macro test**: `(dec 5)` → Should return 4
5. **SQUARE macro test**: `(square 4)` → Should return 16

### Running Tests
```assembly
JSR RUN_MACRO_TESTS    ; Run all macro tests
JSR TEST_MACRO_EXPANSION ; Run expansion tests
```

## Performance Considerations

### Memory Usage
- **Macro table**: 1KB (16 macros × 64 bytes)
- **Expansion buffer**: Reuses input buffer
- **Stack usage**: Minimal additional overhead

### Execution Speed
- **Macro lookup**: O(n) linear search through macro table
- **Expansion**: Single pass parameter substitution
- **Overall**: ~500-1000 additional cycles per macro call

## Examples and Use Cases

### Configuration Macros
```lisp
; Define system constants
(defmacro screen-width () 320)
(defmacro screen-height () 200)
```

### Control Flow Macros
```lisp
; While loop simulation
(defmacro while (condition body)
  (list 'if condition (list 'progn body (list 'while condition body))))
```

### Mathematical Macros
```lisp
; Absolute value
(defmacro abs (x)
  (list 'if (list '< x 0) (list '- x) x))

; Maximum of two values
(defmacro max (a b)
  (list 'if (list '> a b) a b))
```

### Debugging Macros
```lisp
; Debug print with label
(defmacro debug-print (label value)
  (list 'progn 
        (list 'print (list 'quote label)) 
        (list 'print value)))
```

## Limitations

### Current Implementation
- **Fixed parameter substitution**: Uses simple placeholder system
- **No gensym**: Cannot generate unique symbols automatically
- **Limited nesting**: Maximum 5 levels of macro expansion
- **Static table**: Fixed number of macros (16 maximum)
- **No persistence**: Macros lost when system resets

### Memory Constraints
- **Template size**: Limited to ~40 characters per macro body
- **Parameter limit**: Maximum ~10 parameters per macro
- **Name length**: Maximum 15 characters for macro names

## Future Enhancements

### Advanced Features
```lisp
; Backquote and comma syntax
`(if ,condition ,body)

; Splice unquote for lists
`(progn ,@body-list)

; Macro hygiene - automatic variable renaming
(defmacro let-unique (var val body)
  (let ((temp-var (gensym)))
    `(let ((,temp-var ,val)) ,body)))
```

### System Integration
- **File-based macros**: Load macro definitions from files
- **Macro libraries**: Standard collections of useful macros
- **Interactive definition**: Define macros at runtime
- **Macro debugging**: Step through expansion process

### Performance Optimizations
- **Hash table lookup**: O(1) macro lookup instead of O(n)
- **Compiled expansion**: Cache expanded forms
- **Partial evaluation**: Optimize constant expressions in macros

## Best Practices

### Macro Design
1. **Keep macros simple**: Complex logic should use functions
2. **Avoid side effects**: Macros should transform code, not execute it
3. **Document parameters**: Clear parameter names and documentation
4. **Test thoroughly**: Ensure expansion produces correct results

### Naming Conventions
- **Use descriptive names**: `when`, `unless`, `square`
- **Avoid conflicts**: Don't shadow built-in functions
- **Consider scope**: Macros are global in this implementation

### Performance Tips
- **Minimize nesting**: Deep macro expansion is expensive
- **Cache results**: Don't re-expand the same macro repeatedly
- **Profile usage**: Identify frequently used macros for optimization

This macro system brings the power of Lisp metaprogramming to the 6502, allowing sophisticated code generation and abstraction within the constraints of 8-bit computing.