; Extended Lisp Parser in 6502 Assembly
; Supports basic arithmetic, comparison, and list operations
; This is an enhanced version with more complete functionality

; Extended zero page variables
SYMBOL_LEN   = $1D    ; Length of current symbol
ARGC         = $1E    ; Argument count for functions
COMPARISON   = $1F    ; Comparison result

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

        .org $8000

; Enhanced main entry point
MAIN:
        JSR INIT_PARSER
        JSR INIT_SYMBOLS
        JSR RUN_TESTS
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

; Enhanced symbol lookup with built-in functions
LOOKUP_SYMBOL:
        ; Get symbol length
        JSR GET_SYMBOL_LENGTH
        
        ; Search symbol table
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