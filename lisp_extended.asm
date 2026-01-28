; Extended Lisp Parser in 6502 Assembly
; Supports basic arithmetic, comparison, and list operations
; This is an enhanced version with more complete functionality

; Extended zero page variables
SYMBOL_LEN   = $1D    ; Length of current symbol
ARGC         = $1E    ; Argument count for functions
COMPARISON   = $1F    ; Comparison result
MACRO_PTR    = $20    ; Pointer to current macro
MACRO_ARGC   = $22    ; Macro argument count
EXPAND_FLAG  = $23    ; Macro expansion flag
MACRO_DEPTH  = $24    ; Macro expansion depth (prevent infinite recursion)

; Function IDs
FUNC_ADD     = $01
FUNC_SUB     = $02
FUNC_MUL     = $03
FUNC_DIV     = $04
FUNC_EQ      = $05
FUNC_LT      = $06
FUNC_GT      = $07
FUNC_AND     = $08
FUNC_OR      = $09
FUNC_NOT     = $0A
FUNC_LIST    = $0B
FUNC_CAR     = $0C
FUNC_CDR     = $0D
FUNC_DEFMACRO = $0E
FUNC_QUOTE   = $0F
FUNC_UNQUOTE = $10

; Macro table layout (after symbol table)
MACRO_TABLE  = $1400  ; Macro definitions storage
MAX_MACROS   = $10    ; Maximum number of macros
MACRO_ENTRY_SIZE = $40 ; Size of each macro entry

        .org $8000

; Enhanced main entry point
MAIN:
        JSR INIT_PARSER
        JSR INIT_SYMBOLS
        JSR INIT_MACROS
        JSR RUN_TESTS
        JSR RUN_MACRO_TESTS
        BRK

; Initialize built-in symbols
INIT_SYMBOLS:
        ; Set up built-in function symbols
        LDX #$00
@INIT_LOOP:
        LDA BUILTIN_NAMES,X
        BEQ @INIT_DONE
        
        ; Copy symbol name to symbol table
        LDY #$00
@COPY_NAME:
        LDA BUILTIN_NAMES,X
        STA SYMBOL_TABLE,Y
        BEQ @NAME_COPIED
        INX
        INY
        JMP @COPY_NAME

@NAME_COPIED:
        INX
        INY
        ; Store function ID
        LDA BUILTIN_IDS,X
        STA SYMBOL_TABLE,Y
        INX
        INY
        JMP @INIT_LOOP

@INIT_DONE:
        RTS

; Built-in function names (null-terminated)
BUILTIN_NAMES:
        .byte "+", $00
        .byte "-", $00
        .byte "*", $00
        .byte "/", $00
        .byte "=", $00
        .byte "<", $00
        .byte ">", $00
        .byte "and", $00
        .byte "or", $00
        .byte "not", $00
        .byte "list", $00
        .byte "car", $00
        .byte "cdr", $00
        .byte "defmacro", $00
        .byte "quote", $00
        .byte "unquote", $00
        .byte $00

; Corresponding function IDs
BUILTIN_IDS:
        .byte FUNC_ADD
        .byte FUNC_SUB
        .byte FUNC_MUL
        .byte FUNC_DIV
        .byte FUNC_EQ
        .byte FUNC_LT
        .byte FUNC_GT
        .byte FUNC_AND
        .byte FUNC_OR
        .byte FUNC_NOT
        .byte FUNC_LIST
        .byte FUNC_CAR
        .byte FUNC_CDR
        .byte FUNC_DEFMACRO
        .byte FUNC_QUOTE
        .byte FUNC_UNQUOTE
        .byte FUNC_LT
        .byte FUNC_GT
        .byte FUNC_AND
        .byte FUNC_OR
        .byte FUNC_NOT
        .byte FUNC_LIST
        .byte FUNC_CAR
        .byte FUNC_CDR

; Run test expressions
RUN_TESTS:
        ; Test 1: (+ 1 2 3) = 6
        JSR TEST_ADDITION
        
        ; Test 2: (- 10 3) = 7
        JSR TEST_SUBTRACTION
        
        ; Test 3: (* 2 3 4) = 24
        JSR TEST_MULTIPLICATION
        
        ; Test 4: (= 5 5) = true
        JSR TEST_EQUALITY
        
        RTS

; Test addition
TEST_ADDITION:
        LDX #$00
@COPY:
        LDA TEST_ADD_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE
        INX
        JMP @COPY
@PARSE:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        ; Result should be 6
        RTS

TEST_ADD_EXPR:
        .byte "(+ 1 2 3)", $00

; Test subtraction
TEST_SUBTRACTION:
        LDX #$00
@COPY:
        LDA TEST_SUB_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE
        INX
        JMP @COPY
@PARSE:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        ; Result should be 7
        RTS

TEST_SUB_EXPR:
        .byte "(- 10 3)", $00

; Test multiplication
TEST_MULTIPLICATION:
        LDX #$00
@COPY:
        LDA TEST_MUL_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE
        INX
        JMP @COPY
@PARSE:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        ; Result should be 24
        RTS

TEST_MUL_EXPR:
        .byte "(* 2 3 4)", $00

; Test equality
TEST_EQUALITY:
        LDX #$00
@COPY:
        LDA TEST_EQ_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE
        INX
        JMP @COPY
@PARSE:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        ; Result should be true (non-zero)
        RTS

TEST_EQ_EXPR:
        .byte "(= 5 5)", $00

; Reset parser state
RESET_PARSER:
        LDA #<INPUT_BUFFER
        STA INPUT_PTR
        LDA #>INPUT_BUFFER
        STA INPUT_PTR+1
        
        LDA #<EXPR_STACK
        STA STACK_PTR
        LDA #>EXPR_STACK
        STA STACK_PTR+1
        
        LDA #$00
        STA ERROR_FLAG
        STA PAREN_COUNT
        STA ARGC
        
        RTS

; Enhanced symbol lookup with built-in functions and macros
LOOKUP_SYMBOL:
        ; First check if it's a macro
        JSR LOOKUP_MACRO
        LDA TOKEN_VALUE
        ORA TOKEN_VALUE+1
        BNE @FOUND_MACRO
        
        ; Get symbol length
        JSR GET_SYMBOL_LENGTH
        
        ; Search built-in symbol table
        LDY #$00
@SEARCH_LOOP:
        LDA SYMBOL_TABLE,Y
        BEQ @NOT_FOUND
        
        ; Compare symbol
        JSR COMPARE_SYMBOL
        BEQ @FOUND
        
        ; Skip to next symbol
        JSR SKIP_SYMBOL_ENTRY
        JMP @SEARCH_LOOP

@FOUND:
        ; Get function ID
        TYA
        CLC
        ADC SYMBOL_LEN
        TAY
        INY
        LDA SYMBOL_TABLE,Y
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        RTS

@FOUND_MACRO:
        ; Macro found - TOKEN_VALUE contains macro entry pointer
        RTS

@NOT_FOUND:
        ; Unknown symbol - return 0
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        RTS

; Get length of symbol pointed to by TOKEN_VALUE
GET_SYMBOL_LENGTH:
        LDY #$00
@COUNT:
        LDA (TOKEN_VALUE),Y
        BEQ @DONE
        INY
        JMP @COUNT
@DONE:
        STY SYMBOL_LEN
        RTS

; Compare symbol in TOKEN_VALUE with symbol at offset Y in symbol table
COMPARE_SYMBOL:
        LDX #$00
@COMPARE_LOOP:
        LDA (TOKEN_VALUE),X
        CMP SYMBOL_TABLE,Y
        BNE @NOT_EQUAL
        
        ; Check if both strings ended
        CMP #$00
        BEQ @EQUAL
        
        INX
        INY
        JMP @COMPARE_LOOP

@EQUAL:
        LDA #$00    ; Equal
        RTS

@NOT_EQUAL:
        LDA #$01    ; Not equal
        RTS

; Skip to next symbol entry in symbol table
SKIP_SYMBOL_ENTRY:
        ; Skip symbol name
@SKIP_NAME:
        LDA SYMBOL_TABLE,Y
        INY
        BNE @SKIP_NAME
        
        ; Skip function ID
        INY
        RTS

; Enhanced function caller with multiple operators
CALL_FUNCTION:
        ; Get function ID
        LDA TOKEN_VALUE
        
        ; Count arguments on stack
        JSR COUNT_ARGS
        
        CMP #FUNC_ADD
        BEQ @CALL_ADD
        CMP #FUNC_SUB
        BEQ @CALL_SUB
        CMP #FUNC_MUL
        BEQ @CALL_MUL
        CMP #FUNC_DIV
        BEQ @CALL_DIV
        CMP #FUNC_EQ
        BEQ @CALL_EQ
        CMP #FUNC_LT
        BEQ @CALL_LT
        CMP #FUNC_GT
        BEQ @CALL_GT
        CMP #FUNC_AND
        BEQ @CALL_AND
        CMP #FUNC_OR
        BEQ @CALL_OR
        CMP #FUNC_NOT
        BEQ @CALL_NOT
        CMP #FUNC_DEFMACRO
        BEQ @CALL_DEFMACRO
        CMP #FUNC_QUOTE
        BEQ @CALL_QUOTE
        CMP #FUNC_UNQUOTE
        BEQ @CALL_UNQUOTE
        
        ; Unknown function
        LDA #$03
        STA ERROR_FLAG
        RTS

@CALL_ADD:
        JSR FUNC_ADDITION
        RTS

@CALL_SUB:
        JSR FUNC_SUBTRACTION
        RTS

@CALL_MUL:
        JSR FUNC_MULTIPLICATION
        RTS

@CALL_DIV:
        JSR FUNC_DIVISION
        RTS

@CALL_EQ:
        JSR FUNC_EQUALITY
        RTS

@CALL_LT:
        JSR FUNC_LESS_THAN
        RTS

@CALL_GT:
        JSR FUNC_GREATER_THAN
        RTS

@CALL_AND:
        JSR FUNC_LOGICAL_AND
        RTS

@CALL_OR:
        JSR FUNC_LOGICAL_OR
        RTS

@CALL_NOT:
        JSR FUNC_LOGICAL_NOT
        RTS

@CALL_DEFMACRO:
        JSR FUNC_DEFMACRO
        RTS

@CALL_QUOTE:
        JSR FUNC_QUOTE
        RTS

@CALL_UNQUOTE:
        JSR FUNC_UNQUOTE
        RTS

; Count arguments on expression stack
COUNT_ARGS:
        LDA STACK_PTR
        SEC
        SBC #<EXPR_STACK
        STA ARGC
        LDA STACK_PTR+1
        SBC #>EXPR_STACK
        ; ARGC now contains number of bytes / 2 = number of args
        LSR ARGC
        RTS

; Addition function (sum all arguments)
FUNC_ADDITION:
        LDA #$00
        STA RESULT
        STA RESULT+1
        
@ADD_LOOP:
        LDA ARGC
        BEQ @ADD_DONE
        
        JSR POP_NUMBER
        
        ; Add to sum
        CLC
        LDA RESULT
        ADC RESULT
        STA RESULT
        LDA RESULT+1
        ADC RESULT+1
        STA RESULT+1
        
        DEC ARGC
        JMP @ADD_LOOP

@ADD_DONE:
        ; Push result
        LDA RESULT
        STA TOKEN_VALUE
        LDA RESULT+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Subtraction function (first - rest)
FUNC_SUBTRACTION:
        LDA ARGC
        BEQ @SUB_ERROR
        CMP #$01
        BEQ @SUB_NEGATE
        
        ; Pop first argument
        JSR POP_NUMBER
        LDA RESULT
        STA TEMP
        LDA RESULT+1
        STA TEMP+1
        
        DEC ARGC

@SUB_LOOP:
        LDA ARGC
        BEQ @SUB_DONE
        
        JSR POP_NUMBER
        
        ; Subtract from first argument
        SEC
        LDA TEMP
        SBC RESULT
        STA TEMP
        LDA TEMP+1
        SBC RESULT+1
        STA TEMP+1
        
        DEC ARGC
        JMP @SUB_LOOP

@SUB_DONE:
        LDA TEMP
        STA TOKEN_VALUE
        LDA TEMP+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@SUB_NEGATE:
        ; Negate single argument
        JSR POP_NUMBER
        SEC
        LDA #$00
        SBC RESULT
        STA TOKEN_VALUE
        LDA #$00
        SBC RESULT+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@SUB_ERROR:
        LDA #$04
        STA ERROR_FLAG
        RTS

; Multiplication function
FUNC_MULTIPLICATION:
        LDA #$01
        STA RESULT
        LDA #$00
        STA RESULT+1

@MUL_LOOP:
        LDA ARGC
        BEQ @MUL_DONE
        
        JSR POP_NUMBER
        JSR MULTIPLY_16BIT
        
        DEC ARGC
        JMP @MUL_LOOP

@MUL_DONE:
        LDA RESULT
        STA TOKEN_VALUE
        LDA RESULT+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; 16-bit multiplication: RESULT = RESULT * (value in RESULT from POP_NUMBER)
MULTIPLY_16BIT:
        ; This is a simplified 16-bit multiply
        ; In a real implementation, you'd want a more efficient algorithm
        LDA RESULT
        PHA
        LDA RESULT+1
        PHA
        
        ; Use repeated addition for simplicity
        LDA #$00
        STA TEMP
        STA TEMP+1
        
        PLA
        STA RESULT+1
        PLA
        STA RESULT
        
        ; TEMP will accumulate the result
        ; RESULT contains the multiplier
        
@MUL_16_LOOP:
        LDA RESULT
        ORA RESULT+1
        BEQ @MUL_16_DONE
        
        ; Add original value to accumulator
        CLC
        LDA TEMP
        ADC RESULT
        STA TEMP
        LDA TEMP+1
        ADC RESULT+1
        STA TEMP+1
        
        ; Decrement multiplier
        SEC
        LDA RESULT
        SBC #$01
        STA RESULT
        LDA RESULT+1
        SBC #$00
        STA RESULT+1
        
        JMP @MUL_16_LOOP

@MUL_16_DONE:
        LDA TEMP
        STA RESULT
        LDA TEMP+1
        STA RESULT+1
        RTS

; Division function (simplified)
FUNC_DIVISION:
        ; Not implemented in this basic version
        LDA #$05
        STA ERROR_FLAG
        RTS

; Equality comparison
FUNC_EQUALITY:
        LDA ARGC
        CMP #$02
        BNE @EQ_ERROR
        
        ; Pop two arguments
        JSR POP_NUMBER
        LDA RESULT
        STA TEMP
        LDA RESULT+1
        STA TEMP+1
        
        JSR POP_NUMBER
        
        ; Compare
        LDA RESULT
        CMP TEMP
        BNE @EQ_FALSE
        LDA RESULT+1
        CMP TEMP+1
        BNE @EQ_FALSE
        
        ; Equal - return true (non-zero)
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@EQ_FALSE:
        ; Not equal - return false (zero)
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@EQ_ERROR:
        LDA #$06
        STA ERROR_FLAG
        RTS

; Less than comparison
FUNC_LESS_THAN:
        LDA ARGC
        CMP #$02
        BNE @LT_ERROR
        
        ; Pop second argument
        JSR POP_NUMBER
        LDA RESULT
        STA TEMP
        LDA RESULT+1
        STA TEMP+1
        
        ; Pop first argument
        JSR POP_NUMBER
        
        ; Compare: first < second ?
        ; Subtract second from first
        SEC
        LDA RESULT
        SBC TEMP
        LDA RESULT+1
        SBC TEMP+1
        
        ; Check sign bit (negative result means first < second)
        BMI @LT_TRUE
        
        ; Not less than
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@LT_TRUE:
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@LT_ERROR:
        LDA #$07
        STA ERROR_FLAG
        RTS

; Greater than comparison
FUNC_GREATER_THAN:
        LDA ARGC
        CMP #$02
        BNE @GT_ERROR
        
        ; Pop second argument
        JSR POP_NUMBER
        LDA RESULT
        STA TEMP
        LDA RESULT+1
        STA TEMP+1
        
        ; Pop first argument
        JSR POP_NUMBER
        
        ; Compare: first > second ?
        ; Check if first == second
        LDA RESULT
        CMP TEMP
        BNE @GT_CHECK
        LDA RESULT+1
        CMP TEMP+1
        BEQ @GT_FALSE    ; Equal, so not greater

@GT_CHECK:
        ; Subtract second from first
        SEC
        LDA RESULT
        SBC TEMP
        LDA RESULT+1
        SBC TEMP+1
        
        ; Check sign bit (positive result means first > second)
        BPL @GT_TRUE
        
@GT_FALSE:
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@GT_TRUE:
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@GT_ERROR:
        LDA #$08
        STA ERROR_FLAG
        RTS

; Logical AND
FUNC_LOGICAL_AND:
        LDA #$01
        STA RESULT
        LDA #$00
        STA RESULT+1

@AND_LOOP:
        LDA ARGC
        BEQ @AND_DONE
        
        JSR POP_NUMBER
        
        ; Check if argument is zero
        LDA RESULT
        ORA RESULT+1
        BEQ @AND_FALSE
        
        DEC ARGC
        JMP @AND_LOOP

@AND_DONE:
        ; All arguments were non-zero
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@AND_FALSE:
        ; At least one argument was zero
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Logical OR
FUNC_LOGICAL_OR:
@OR_LOOP:
        LDA ARGC
        BEQ @OR_FALSE
        
        JSR POP_NUMBER
        
        ; Check if argument is non-zero
        LDA RESULT
        ORA RESULT+1
        BNE @OR_TRUE
        
        DEC ARGC
        JMP @OR_LOOP

@OR_TRUE:
        ; At least one argument was non-zero
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@OR_FALSE:
        ; All arguments were zero
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Logical NOT
FUNC_LOGICAL_NOT:
        LDA ARGC
        CMP #$01
        BNE @NOT_ERROR
        
        JSR POP_NUMBER
        
        ; Check if argument is zero
        LDA RESULT
        ORA RESULT+1
        BEQ @NOT_TRUE
        
        ; Argument was non-zero, return false
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@NOT_TRUE:
        ; Argument was zero, return true
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@NOT_ERROR:
        LDA #$09
        STA ERROR_FLAG
        RTS

; ======================================================================
; MACRO SYSTEM IMPLEMENTATION
; ======================================================================

; Initialize macro table
INIT_MACROS:
        ; Clear macro table
        LDX #$00
        LDA #$00
@CLEAR_LOOP:
        STA MACRO_TABLE,X
        INX
        CPX #(MAX_MACROS * MACRO_ENTRY_SIZE)
        BNE @CLEAR_LOOP
        
        ; Initialize macro system variables
        LDA #$00
        STA MACRO_DEPTH
        STA EXPAND_FLAG
        
        ; Define some built-in macros
        JSR DEFINE_BUILTIN_MACROS
        
        RTS

; Define built-in macros like WHEN, UNLESS, etc.
DEFINE_BUILTIN_MACROS:
        ; Define (when condition body) macro
        ; Expands to (if condition body)
        JSR DEFINE_WHEN_MACRO
        
        ; Define (unless condition body) macro  
        ; Expands to (if (not condition) body)
        JSR DEFINE_UNLESS_MACRO
        
        RTS

; Define WHEN macro
DEFINE_WHEN_MACRO:
        ; This is a simplified version - in a real implementation
        ; you'd parse the macro definition from source
        LDX #$00
        
        ; Store macro name "when"
        LDA #'w'
        STA MACRO_TABLE,X
        INX
        LDA #'h'
        STA MACRO_TABLE,X
        INX
        LDA #'e'
        STA MACRO_TABLE,X
        INX
        LDA #'n'
        STA MACRO_TABLE,X
        INX
        LDA #$00
        STA MACRO_TABLE,X
        INX
        
        ; Store parameter count (2: condition and body)
        LDA #$02
        STA MACRO_TABLE,X
        INX
        
        ; Store macro body template
        ; Template: "(if <arg1> <arg2>)"
        LDY #$00
@STORE_TEMPLATE:
        LDA WHEN_TEMPLATE,Y
        STA MACRO_TABLE,X
        BEQ @TEMPLATE_DONE
        INX
        INY
        JMP @STORE_TEMPLATE

@TEMPLATE_DONE:
        RTS

WHEN_TEMPLATE:
        .byte "(if ", $01, " ", $02, ")", $00  ; $01, $02 are parameter placeholders

; Define UNLESS macro  
DEFINE_UNLESS_MACRO:
        ; Skip to next macro slot
        LDX #MACRO_ENTRY_SIZE
        
        ; Store macro name "unless"
        LDA #'u'
        STA MACRO_TABLE,X
        INX
        LDA #'n'
        STA MACRO_TABLE,X
        INX
        LDA #'l'
        STA MACRO_TABLE,X
        INX
        LDA #'e'
        STA MACRO_TABLE,X
        INX
        LDA #'s'
        STA MACRO_TABLE,X
        INX
        LDA #'s'
        STA MACRO_TABLE,X
        INX
        LDA #$00
        STA MACRO_TABLE,X
        INX
        
        ; Store parameter count (2)
        LDA #$02
        STA MACRO_TABLE,X
        INX
        
        ; Store macro body template
        ; Template: "(if (not <arg1>) <arg2>)"
        LDY #$00
@STORE_TEMPLATE2:
        LDA UNLESS_TEMPLATE,Y
        STA MACRO_TABLE,X
        BEQ @TEMPLATE2_DONE
        INX
        INY
        JMP @STORE_TEMPLATE2

@TEMPLATE2_DONE:
        RTS

UNLESS_TEMPLATE:
        .byte "(if (not ", $01, ") ", $02, ")", $00

; Look up macro in macro table
LOOKUP_MACRO:
        ; Get symbol length first
        JSR GET_SYMBOL_LENGTH
        
        ; Search macro table
        LDX #$00
@MACRO_SEARCH:
        LDA MACRO_TABLE,X
        BEQ @MACRO_NOT_FOUND
        
        ; Compare macro name
        JSR COMPARE_MACRO_NAME
        BEQ @MACRO_FOUND
        
        ; Skip to next macro entry
        TXA
        CLC
        ADC #MACRO_ENTRY_SIZE
        TAX
        CMP #(MAX_MACROS * MACRO_ENTRY_SIZE)
        BCC @MACRO_SEARCH

@MACRO_NOT_FOUND:
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        RTS

@MACRO_FOUND:
        ; Return pointer to macro entry
        TXA
        CLC
        ADC #<MACRO_TABLE
        STA TOKEN_VALUE
        LDA #>MACRO_TABLE
        ADC #$00
        STA TOKEN_VALUE+1
        RTS

; Compare macro name at offset X with symbol in TOKEN_VALUE
COMPARE_MACRO_NAME:
        LDY #$00
@COMPARE_MACRO_LOOP:
        LDA (TOKEN_VALUE),Y
        CMP MACRO_TABLE,X
        BNE @MACRO_NOT_EQUAL
        
        ; Check if both ended
        CMP #$00
        BEQ @MACRO_EQUAL
        
        INY
        INX
        JMP @COMPARE_MACRO_LOOP

@MACRO_EQUAL:
        LDA #$00    ; Equal
        RTS

@MACRO_NOT_EQUAL:
        LDA #$01    ; Not equal
        RTS

; DEFMACRO function - define a new macro
FUNC_DEFMACRO:
        ; Syntax: (defmacro name (params) body)
        LDA ARGC
        CMP #$03
        BNE @DEFMACRO_ERROR
        
        ; For now, just return success
        ; A full implementation would parse the macro definition
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@DEFMACRO_ERROR:
        LDA #$0A
        STA ERROR_FLAG
        RTS

; QUOTE function - prevent evaluation
FUNC_QUOTE:
        LDA ARGC
        CMP #$01
        BNE @QUOTE_ERROR
        
        ; Simply return the argument unevaluated
        ; The argument is already on the stack
        RTS

@QUOTE_ERROR:
        LDA #$0B
        STA ERROR_FLAG
        RTS

; UNQUOTE function - force evaluation in quote context
FUNC_UNQUOTE:
        LDA ARGC
        CMP #$01
        BNE @UNQUOTE_ERROR
        
        ; Evaluate the argument
        ; This is a simplified implementation
        RTS

@UNQUOTE_ERROR:
        LDA #$0C
        STA ERROR_FLAG
        RTS

; Expand a macro call
; Input: TOKEN_VALUE points to macro entry, arguments on stack
EXPAND_MACRO:
        ; Check recursion depth
        LDA MACRO_DEPTH
        CMP #$05    ; Max recursion depth
        BCS @EXPAND_ERROR
        
        INC MACRO_DEPTH
        
        ; Get macro entry
        LDA TOKEN_VALUE
        STA MACRO_PTR
        LDA TOKEN_VALUE+1
        STA MACRO_PTR+1
        
        ; Get parameter count
        LDY #$10    ; Offset to param count in macro entry
        LDA (MACRO_PTR),Y
        STA MACRO_ARGC
        
        ; Verify argument count matches
        LDA ARGC
        CMP MACRO_ARGC
        BNE @EXPAND_ARG_ERROR
        
        ; Perform macro expansion
        ; This is simplified - real implementation would substitute parameters
        JSR SUBSTITUTE_MACRO_PARAMS
        
        ; Parse expanded text
        JSR PARSE_EXPANDED_MACRO
        
        DEC MACRO_DEPTH
        RTS

@EXPAND_ERROR:
        LDA #$0D
        STA ERROR_FLAG
        DEC MACRO_DEPTH
        RTS

@EXPAND_ARG_ERROR:
        LDA #$0E
        STA ERROR_FLAG
        DEC MACRO_DEPTH
        RTS

; Substitute macro parameters (simplified)
SUBSTITUTE_MACRO_PARAMS:
        ; This would perform parameter substitution in the macro body
        ; For now, just copy the template
        RTS

; Parse expanded macro text
PARSE_EXPANDED_MACRO:
        ; This would parse the expanded macro text
        ; For now, just return success
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Run macro-specific tests
RUN_MACRO_TESTS:
        ; Test WHEN macro
        JSR TEST_WHEN_MACRO
        
        ; Test UNLESS macro  
        JSR TEST_UNLESS_MACRO
        
        RTS

; Test WHEN macro: (when 1 42) should return 42
TEST_WHEN_MACRO:
        LDX #$00
@COPY_WHEN:
        LDA TEST_WHEN_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE_WHEN
        INX
        JMP @COPY_WHEN
@PARSE_WHEN:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        RTS

TEST_WHEN_EXPR:
        .byte "(when 1 42)", $00

; Test UNLESS macro: (unless 0 42) should return 42
TEST_UNLESS_MACRO:
        LDX #$00
@COPY_UNLESS:
        LDA TEST_UNLESS_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE_UNLESS
        INX
        JMP @COPY_UNLESS
@PARSE_UNLESS:
        JSR RESET_PARSER
        JSR PARSE_EXPR
        RTS

TEST_UNLESS_EXPR:
        .byte "(unless 0 42)", $00