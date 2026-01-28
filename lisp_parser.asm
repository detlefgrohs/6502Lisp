; Simple Lisp Parser in 6502 Assembly
; Parses basic S-expressions and evaluates simple arithmetic
; Memory layout:
;   $0000-$00FF: Zero page (variables, pointers)
;   $0100-$01FF: Stack
;   $0200-$07FF: Input buffer and workspace
;   $0800-$0FFF: Symbol table
;   $1000-$1FFF: Expression stack
;   $8000-$FFFF: Program code

; Zero page variables
INPUT_PTR    = $10    ; Pointer to current input position
STACK_PTR    = $12    ; Expression stack pointer
TEMP         = $14    ; Temporary storage
RESULT       = $16    ; Result accumulator
TOKEN_TYPE   = $18    ; Current token type
TOKEN_VALUE  = $19    ; Current token value (16-bit)
PAREN_COUNT  = $1B    ; Parentheses nesting level
ERROR_FLAG   = $1C    ; Error flag

; Memory areas
INPUT_BUFFER = $0200  ; Input text buffer
WORKSPACE    = $0400  ; General workspace
SYMBOL_TABLE = $0800  ; Symbol definitions
EXPR_STACK   = $1000  ; Expression evaluation stack

; Token types
TOK_EOF      = $00
TOK_LPAREN   = $01
TOK_RPAREN   = $02
TOK_NUMBER   = $03
TOK_SYMBOL   = $04
TOK_ERROR    = $FF

; ASCII codes
CHAR_SPACE   = $20
CHAR_LPAREN  = $28    ; '('
CHAR_RPAREN  = $29    ; ')'
CHAR_0       = $30    ; '0'
CHAR_9       = $39    ; '9'
CHAR_A       = $41    ; 'A'
CHAR_Z       = $5A    ; 'Z'
CHAR_a       = $61    ; 'a'
CHAR_z       = $7A    ; 'z'

        .org $8000

; Main entry point
MAIN:
        ; Initialize system
        JSR INIT_PARSER
        
        ; Load test expression
        JSR LOAD_TEST_EXPR
        
        ; Parse and evaluate
        JSR PARSE_EXPR
        
        ; Print result
        JSR PRINT_RESULT
        
        ; Halt
        BRK

; Initialize parser state
INIT_PARSER:
        ; Clear zero page variables
        LDA #$00
        STA INPUT_PTR
        STA INPUT_PTR+1
        STA STACK_PTR
        STA STACK_PTR+1
        STA PAREN_COUNT
        STA ERROR_FLAG
        
        ; Set up input pointer
        LDA #<INPUT_BUFFER
        STA INPUT_PTR
        LDA #>INPUT_BUFFER
        STA INPUT_PTR+1
        
        ; Set up expression stack pointer
        LDA #<EXPR_STACK
        STA STACK_PTR
        LDA #>EXPR_STACK
        STA STACK_PTR+1
        
        RTS

; Load a test expression into input buffer
LOAD_TEST_EXPR:
        LDY #$00
        LDX #$00
@COPY_LOOP:
        LDA TEST_EXPR,X
        STA INPUT_BUFFER,Y
        BEQ @DONE
        INY
        INX
        JMP @COPY_LOOP
@DONE:
        RTS

; Test expression: "(+ 1 2 3)"
TEST_EXPR:
        .byte "(+ 1 2 3)", $00

; Main expression parser
PARSE_EXPR:
        JSR SKIP_WHITESPACE
        JSR GET_TOKEN
        
        ; Check for EOF
        LDA TOKEN_TYPE
        CMP #TOK_EOF
        BEQ @EOF
        
        ; Check for left parenthesis (start of expression)
        CMP #TOK_LPAREN
        BEQ @PARSE_LIST
        
        ; Check for number
        CMP #TOK_NUMBER
        BEQ @PARSE_NUMBER
        
        ; Check for symbol
        CMP #TOK_SYMBOL
        BEQ @PARSE_SYMBOL
        
        ; Error - unexpected token
        LDA #$01
        STA ERROR_FLAG
        RTS

@EOF:
        RTS

@PARSE_NUMBER:
        ; Push number onto expression stack
        JSR PUSH_NUMBER
        RTS

@PARSE_SYMBOL:
        ; Look up symbol and push value
        JSR LOOKUP_SYMBOL
        JSR PUSH_NUMBER
        RTS

@PARSE_LIST:
        ; Parse list expression (function call)
        INC PAREN_COUNT
        
        ; Get function name
        JSR SKIP_WHITESPACE
        JSR GET_TOKEN
        
        ; Should be a symbol
        LDA TOKEN_TYPE
        CMP #TOK_SYMBOL
        BNE @LIST_ERROR
        
        ; Store function for later
        LDA TOKEN_VALUE
        PHA
        LDA TOKEN_VALUE+1
        PHA
        
        ; Parse arguments
        JSR PARSE_ARGS
        
        ; Get function back
        PLA
        STA TOKEN_VALUE+1
        PLA
        STA TOKEN_VALUE
        
        ; Call function
        JSR CALL_FUNCTION
        
        ; Expect closing paren
        JSR SKIP_WHITESPACE
        JSR GET_TOKEN
        LDA TOKEN_TYPE
        CMP #TOK_RPAREN
        BNE @LIST_ERROR
        
        DEC PAREN_COUNT
        RTS

@LIST_ERROR:
        LDA #$02
        STA ERROR_FLAG
        RTS

; Parse function arguments
PARSE_ARGS:
@ARG_LOOP:
        JSR SKIP_WHITESPACE
        JSR PEEK_CHAR
        
        ; Check for closing paren
        CMP #CHAR_RPAREN
        BEQ @ARGS_DONE
        
        ; Parse next argument
        JSR PARSE_EXPR
        
        ; Check for error
        LDA ERROR_FLAG
        BNE @ARGS_DONE
        
        JMP @ARG_LOOP

@ARGS_DONE:
        RTS

; Get next token from input
GET_TOKEN:
        JSR SKIP_WHITESPACE
        JSR PEEK_CHAR
        
        ; Check for EOF
        CMP #$00
        BEQ @SET_EOF
        
        ; Check for left paren
        CMP #CHAR_LPAREN
        BEQ @SET_LPAREN
        
        ; Check for right paren
        CMP #CHAR_RPAREN
        BEQ @SET_RPAREN
        
        ; Check for digit (start of number)
        CMP #CHAR_0
        BCC @TRY_SYMBOL
        CMP #CHAR_9+1
        BCC @READ_NUMBER
        
@TRY_SYMBOL:
        ; Try to read as symbol
        JSR READ_SYMBOL
        LDA #TOK_SYMBOL
        STA TOKEN_TYPE
        RTS

@SET_EOF:
        LDA #TOK_EOF
        STA TOKEN_TYPE
        RTS

@SET_LPAREN:
        JSR NEXT_CHAR
        LDA #TOK_LPAREN
        STA TOKEN_TYPE
        RTS

@SET_RPAREN:
        JSR NEXT_CHAR
        LDA #TOK_RPAREN
        STA TOKEN_TYPE
        RTS

@READ_NUMBER:
        JSR READ_NUMBER_TOKEN
        LDA #TOK_NUMBER
        STA TOKEN_TYPE
        RTS

; Read a number token
READ_NUMBER_TOKEN:
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        
@READ_DIGIT:
        JSR PEEK_CHAR
        
        ; Check if digit
        CMP #CHAR_0
        BCC @NUMBER_DONE
        CMP #CHAR_9+1
        BCS @NUMBER_DONE
        
        ; Convert ASCII to digit
        SEC
        SBC #CHAR_0
        STA TEMP
        
        ; Multiply current value by 10
        JSR MULTIPLY_BY_10
        
        ; Add new digit
        CLC
        LDA TOKEN_VALUE
        ADC TEMP
        STA TOKEN_VALUE
        LDA TOKEN_VALUE+1
        ADC #$00
        STA TOKEN_VALUE+1
        
        ; Advance to next character
        JSR NEXT_CHAR
        JMP @READ_DIGIT

@NUMBER_DONE:
        RTS

; Multiply TOKEN_VALUE by 10
MULTIPLY_BY_10:
        ; Save original value
        LDA TOKEN_VALUE
        STA TEMP
        LDA TOKEN_VALUE+1
        STA TEMP+1
        
        ; Multiply by 2 (shift left)
        ASL TOKEN_VALUE
        ROL TOKEN_VALUE+1
        
        ; Multiply by 4 (shift left again)
        ASL TOKEN_VALUE
        ROL TOKEN_VALUE+1
        
        ; Add original * 2
        ASL TEMP
        ROL TEMP+1
        
        CLC
        LDA TOKEN_VALUE
        ADC TEMP
        STA TOKEN_VALUE
        LDA TOKEN_VALUE+1
        ADC TEMP+1
        STA TOKEN_VALUE+1
        
        ; Result is now original * 10
        RTS

; Read a symbol token
READ_SYMBOL:
        LDY #$00
        
@READ_CHAR:
        JSR PEEK_CHAR
        
        ; Check for end of symbol
        CMP #CHAR_SPACE
        BEQ @SYMBOL_DONE
        CMP #CHAR_LPAREN
        BEQ @SYMBOL_DONE
        CMP #CHAR_RPAREN
        BEQ @SYMBOL_DONE
        CMP #$00
        BEQ @SYMBOL_DONE
        
        ; Store character
        STA WORKSPACE,Y
        INY
        
        ; Advance input
        JSR NEXT_CHAR
        JMP @READ_CHAR

@SYMBOL_DONE:
        ; Null terminate
        LDA #$00
        STA WORKSPACE,Y
        
        ; Store pointer to symbol
        LDA #<WORKSPACE
        STA TOKEN_VALUE
        LDA #>WORKSPACE
        STA TOKEN_VALUE+1
        
        RTS

; Skip whitespace characters
SKIP_WHITESPACE:
        JSR PEEK_CHAR
        CMP #CHAR_SPACE
        BNE @DONE
        JSR NEXT_CHAR
        JMP SKIP_WHITESPACE
@DONE:
        RTS

; Peek at current character without advancing
PEEK_CHAR:
        LDY #$00
        LDA (INPUT_PTR),Y
        RTS

; Get next character and advance pointer
NEXT_CHAR:
        LDY #$00
        LDA (INPUT_PTR),Y
        INC INPUT_PTR
        BNE @NO_CARRY
        INC INPUT_PTR+1
@NO_CARRY:
        RTS

; Push number onto expression stack
PUSH_NUMBER:
        LDY #$00
        LDA TOKEN_VALUE
        STA (STACK_PTR),Y
        INY
        LDA TOKEN_VALUE+1
        STA (STACK_PTR),Y
        
        ; Advance stack pointer
        CLC
        LDA STACK_PTR
        ADC #$02
        STA STACK_PTR
        LDA STACK_PTR+1
        ADC #$00
        STA STACK_PTR+1
        
        RTS

; Pop number from expression stack
POP_NUMBER:
        ; Move stack pointer back
        SEC
        LDA STACK_PTR
        SBC #$02
        STA STACK_PTR
        LDA STACK_PTR+1
        SBC #$00
        STA STACK_PTR+1
        
        ; Read value
        LDY #$00
        LDA (STACK_PTR),Y
        STA RESULT
        INY
        LDA (STACK_PTR),Y
        STA RESULT+1
        
        RTS

; Look up symbol (simplified - just handles operators)
LOOKUP_SYMBOL:
        ; For now, just return 0 for unknown symbols
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        RTS

; Call function (simplified - just handles +)
CALL_FUNCTION:
        ; Check if it's the '+' operator
        LDY #$00
        LDA (TOKEN_VALUE),Y
        CMP #'+'
        BEQ @ADD_FUNCTION
        
        ; Unknown function - error
        LDA #$03
        STA ERROR_FLAG
        RTS

@ADD_FUNCTION:
        ; Initialize sum to 0
        LDA #$00
        STA RESULT
        STA RESULT+1
        
        ; Add all numbers on stack
@ADD_LOOP:
        ; Check if we have more numbers
        LDA STACK_PTR
        CMP #<EXPR_STACK
        BNE @HAVE_NUMBER
        LDA STACK_PTR+1
        CMP #>EXPR_STACK
        BEQ @ADD_DONE
        
@HAVE_NUMBER:
        JSR POP_NUMBER
        
        ; Add to sum
        CLC
        LDA RESULT
        ADC RESULT
        STA TEMP
        LDA RESULT+1
        ADC RESULT+1
        STA TEMP+1
        
        ; Store back in RESULT
        LDA TEMP
        STA RESULT
        LDA TEMP+1
        STA RESULT+1
        
        JMP @ADD_LOOP

@ADD_DONE:
        ; Push result back onto stack
        LDA RESULT
        STA TOKEN_VALUE
        LDA RESULT+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        
        RTS

; Print result (simplified - just stores in RESULT)
PRINT_RESULT:
        JSR POP_NUMBER
        ; RESULT now contains the final result
        ; In a real system, you'd output this to screen/terminal
        RTS

; Interrupt vectors
        .org $FFFA
        .word $0000    ; NMI
        .word MAIN     ; Reset
        .word $0000    ; IRQ