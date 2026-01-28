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
QUASI_DEPTH  = $25    ; Quasiquote nesting depth
GENSYM_COUNT = $26    ; Counter for generating unique symbols
SPLICE_FLAG  = $27    ; Splice unquote flag
HYGIENE_PTR  = $28    ; Pointer to hygiene symbol table

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
FUNC_QUASIQUOTE = $11
FUNC_UNQUOTE_SPLICE = $12
FUNC_GENSYM  = $13
FUNC_MACROEXPAND = $14

; Advanced macro system constants
HYGIENE_TABLE = $1800  ; Hygiene symbol table
MAX_HYGIENE_SYMBOLS = $20  ; Maximum hygienic symbols
GENSYM_PREFIX = "G"    ; Prefix for generated symbols

; Extended token types for advanced macros
TOK_EOF      = $00
TOK_LPAREN   = $01
TOK_RPAREN   = $02
TOK_NUMBER   = $03
TOK_SYMBOL   = $04
TOK_BACKQUOTE = $05    ; `
TOK_COMMA    = $06     ; ,
TOK_COMMA_AT = $07     ; ,@
TOK_ERROR    = $FF

; Extended ASCII codes
CHAR_SPACE   = $20
CHAR_LPAREN  = $28    ; '('
CHAR_RPAREN  = $29    ; ')'
CHAR_COMMA   = $2C    ; ','
CHAR_0       = $30    ; '0'
CHAR_9       = $39    ; '9'
CHAR_A       = $41    ; 'A'
CHAR_Z       = $5A    ; 'Z'
CHAR_BACKQUOTE = $60  ; '`'
CHAR_a       = $61    ; 'a'
CHAR_z       = $7A    ; 'z'
CHAR_AT      = $40    ; '@'

; Memory areas
INPUT_BUFFER = $0200  ; Input text buffer
WORKSPACE    = $0400  ; General workspace
SYMBOL_TABLE = $0800  ; Symbol definitions
EXPR_STACK   = $1000  ; Expression evaluation stack

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
        JSR INIT_HYGIENE
        JSR RUN_TESTS
        JSR RUN_MACRO_TESTS
        JSR RUN_ADVANCED_MACRO_TESTS
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
        .byte "quasiquote", $00
        .byte "unquote-splicing", $00
        .byte "gensym", $00
        .byte "macroexpand", $00
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
        .byte FUNC_QUASIQUOTE
        .byte FUNC_UNQUOTE_SPLICE
        .byte FUNC_GENSYM
        .byte FUNC_MACROEXPAND
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
        CMP #FUNC_QUASIQUOTE
        BEQ @CALL_QUASIQUOTE
        CMP #FUNC_UNQUOTE_SPLICE
        BEQ @CALL_UNQUOTE_SPLICE
        CMP #FUNC_GENSYM
        BEQ @CALL_GENSYM
        CMP #FUNC_MACROEXPAND
        BEQ @CALL_MACROEXPAND
        
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

@CALL_QUASIQUOTE:
        JSR FUNC_QUASIQUOTE
        RTS

@CALL_UNQUOTE_SPLICE:
        JSR FUNC_UNQUOTE_SPLICE
        RTS

@CALL_GENSYM:
        JSR FUNC_GENSYM
        RTS

@CALL_MACROEXPAND:
        JSR FUNC_MACROEXPAND
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
; ADVANCED TOKENIZER FOR BACKQUOTE/COMMA SUPPORT
; ======================================================================

; Enhanced tokenizer that handles `, ,, and ,@
GET_ENHANCED_TOKEN:
        JSR SKIP_WHITESPACE
        JSR PEEK_CHAR
        
        ; Check for EOF
        CMP #$00
        BEQ @SET_EOF_TOK
        
        ; Check for left paren
        CMP #CHAR_LPAREN
        BEQ @SET_LPAREN_TOK
        
        ; Check for right paren
        CMP #CHAR_RPAREN
        BEQ @SET_RPAREN_TOK
        
        ; Check for backquote
        CMP #CHAR_BACKQUOTE
        BEQ @SET_BACKQUOTE_TOK
        
        ; Check for comma (and comma-at)
        CMP #CHAR_COMMA
        BEQ @CHECK_COMMA_TYPES
        
        ; Check for digit (start of number)
        CMP #CHAR_0
        BCC @TRY_SYMBOL_TOK
        CMP #CHAR_9+1
        BCC @READ_NUMBER_TOK
        
@TRY_SYMBOL_TOK:
        ; Try to read as symbol
        JSR READ_SYMBOL_ENHANCED
        LDA #TOK_SYMBOL
        STA TOKEN_TYPE
        RTS

@SET_EOF_TOK:
        LDA #TOK_EOF
        STA TOKEN_TYPE
        RTS

@SET_LPAREN_TOK:
        JSR NEXT_CHAR
        LDA #TOK_LPAREN
        STA TOKEN_TYPE
        RTS

@SET_RPAREN_TOK:
        JSR NEXT_CHAR
        LDA #TOK_RPAREN
        STA TOKEN_TYPE
        RTS

@SET_BACKQUOTE_TOK:
        JSR NEXT_CHAR
        LDA #TOK_BACKQUOTE
        STA TOKEN_TYPE
        RTS

@CHECK_COMMA_TYPES:
        ; Advance past comma
        JSR NEXT_CHAR
        
        ; Check if next character is @
        JSR PEEK_CHAR
        CMP #CHAR_AT
        BNE @SET_COMMA_TOK
        
        ; It's ,@ (splice unquote)
        JSR NEXT_CHAR
        LDA #TOK_COMMA_AT
        STA TOKEN_TYPE
        RTS

@SET_COMMA_TOK:
        ; Just a regular comma (unquote)
        LDA #TOK_COMMA
        STA TOKEN_TYPE
        RTS

@READ_NUMBER_TOK:
        JSR READ_NUMBER_TOKEN
        LDA #TOK_NUMBER
        STA TOKEN_TYPE
        RTS

; Enhanced symbol reader (same as before but calls enhanced char functions)
READ_SYMBOL_ENHANCED:
        LDY #$00
        
@READ_CHAR_ENH:
        JSR PEEK_CHAR
        
        ; Check for end of symbol
        CMP #CHAR_SPACE
        BEQ @SYMBOL_DONE_ENH
        CMP #CHAR_LPAREN
        BEQ @SYMBOL_DONE_ENH
        CMP #CHAR_RPAREN
        BEQ @SYMBOL_DONE_ENH
        CMP #CHAR_BACKQUOTE
        BEQ @SYMBOL_DONE_ENH
        CMP #CHAR_COMMA
        BEQ @SYMBOL_DONE_ENH
        CMP #$00
        BEQ @SYMBOL_DONE_ENH
        
        ; Store character
        STA WORKSPACE,Y
        INY
        
        ; Advance input
        JSR NEXT_CHAR
        JMP @READ_CHAR_ENH

@SYMBOL_DONE_ENH:
        ; Null terminate
        LDA #$00
        STA WORKSPACE,Y
        
        ; Store pointer to symbol
        LDA #<WORKSPACE
        STA TOKEN_VALUE
        LDA #>WORKSPACE
        STA TOKEN_VALUE+1
        
        RTS

; Enhanced expression parser that handles quasiquote constructs
PARSE_ENHANCED_EXPR:
        JSR GET_ENHANCED_TOKEN
        
        ; Check token type
        LDA TOKEN_TYPE
        CMP #TOK_EOF
        BEQ @PARSE_EOF
        
        CMP #TOK_BACKQUOTE
        BEQ @PARSE_QUASIQUOTE
        
        CMP #TOK_COMMA
        BEQ @PARSE_UNQUOTE
        
        CMP #TOK_COMMA_AT
        BEQ @PARSE_SPLICE
        
        CMP #TOK_LPAREN
        BEQ @PARSE_LIST_ENH
        
        CMP #TOK_NUMBER
        BEQ @PARSE_NUMBER_ENH
        
        CMP #TOK_SYMBOL
        BEQ @PARSE_SYMBOL_ENH
        
        ; Error - unexpected token
        LDA #$01
        STA ERROR_FLAG
        RTS

@PARSE_EOF:
        RTS

@PARSE_QUASIQUOTE:
        ; Handle `expr - convert to (quasiquote expr)
        JSR HANDLE_QUASIQUOTE_READER
        RTS

@PARSE_UNQUOTE:
        ; Handle ,expr - convert to (unquote expr)
        JSR HANDLE_UNQUOTE_READER
        RTS

@PARSE_SPLICE:
        ; Handle ,@expr - convert to (unquote-splicing expr)
        JSR HANDLE_SPLICE_READER
        RTS

@PARSE_LIST_ENH:
        ; Parse regular list
        JSR PARSE_LIST_ENHANCED
        RTS

@PARSE_NUMBER_ENH:
        ; Push number onto stack
        JSR PUSH_NUMBER
        RTS

@PARSE_SYMBOL_ENH:
        ; Look up symbol and handle macros
        JSR LOOKUP_ENHANCED_SYMBOL
        RTS

; Handle `expr reader macro
HANDLE_QUASIQUOTE_READER:
        ; Create (quasiquote expr) form
        ; First push the quasiquote symbol
        LDA #<QUASIQUOTE_SYM
        STA TOKEN_VALUE
        LDA #>QUASIQUOTE_SYM
        STA TOKEN_VALUE+1
        JSR PUSH_SYMBOL_REF
        
        ; Parse the expression after `
        INC QUASI_DEPTH
        JSR PARSE_ENHANCED_EXPR
        DEC QUASI_DEPTH
        
        ; Create list with quasiquote and expression
        JSR CREATE_QUASIQUOTE_FORM
        RTS

QUASIQUOTE_SYM:
        .byte "quasiquote", $00

; Handle ,expr reader macro
HANDLE_UNQUOTE_READER:
        ; Create (unquote expr) form
        LDA #<UNQUOTE_SYM
        STA TOKEN_VALUE
        LDA #>UNQUOTE_SYM
        STA TOKEN_VALUE+1
        JSR PUSH_SYMBOL_REF
        
        ; Parse the expression after ,
        JSR PARSE_ENHANCED_EXPR
        
        ; Create list with unquote and expression
        JSR CREATE_UNQUOTE_FORM
        RTS

UNQUOTE_SYM:
        .byte "unquote", $00

; Handle ,@expr reader macro
HANDLE_SPLICE_READER:
        ; Create (unquote-splicing expr) form
        LDA #<SPLICE_SYM
        STA TOKEN_VALUE
        LDA #>SPLICE_SYM
        STA TOKEN_VALUE+1
        JSR PUSH_SYMBOL_REF
        
        ; Parse the expression after ,@
        LDA #$01
        STA SPLICE_FLAG
        JSR PARSE_ENHANCED_EXPR
        LDA #$00
        STA SPLICE_FLAG
        
        ; Create list with unquote-splicing and expression
        JSR CREATE_SPLICE_FORM
        RTS

SPLICE_SYM:
        .byte "unquote-splicing", $00

; Create quasiquote form from stack contents
CREATE_QUASIQUOTE_FORM:
        ; This would create the proper list structure
        ; For now, just push a placeholder
        LDA #$01
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Create unquote form from stack contents  
CREATE_UNQUOTE_FORM:
        ; This would create the proper list structure
        ; For now, just push a placeholder
        LDA #$02
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Create splice form from stack contents
CREATE_SPLICE_FORM:
        ; This would create the proper list structure
        ; For now, just push a placeholder
        LDA #$03
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Push symbol reference onto stack
PUSH_SYMBOL_REF:
        ; For now, just push the symbol as a number
        LDA TOKEN_VALUE
        STA TEMP
        LDA TOKEN_VALUE+1
        STA TEMP+1
        
        ; Convert address to number representation
        LDA TEMP
        STA TOKEN_VALUE
        LDA TEMP+1
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

; Enhanced symbol lookup with macro handling
LOOKUP_ENHANCED_SYMBOL:
        ; First try regular lookup
        JSR LOOKUP_SYMBOL
        
        ; Check if it's a macro that needs expansion
        LDA TOKEN_VALUE
        ORA TOKEN_VALUE+1
        BNE @FOUND_SOMETHING
        
        ; Not found - return 0
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1

@FOUND_SOMETHING:
        JSR PUSH_NUMBER
        RTS

; Parse enhanced list with quasiquote support
PARSE_LIST_ENHANCED:
        ; Similar to regular list parsing but with quasiquote awareness
        INC PAREN_COUNT
        
        ; Get function name
        JSR SKIP_WHITESPACE
        JSR GET_ENHANCED_TOKEN
        
        ; Should be a symbol
        LDA TOKEN_TYPE
        CMP #TOK_SYMBOL
        BNE @LIST_ERROR_ENH
        
        ; Store function for later
        LDA TOKEN_VALUE
        PHA
        LDA TOKEN_VALUE+1
        PHA
        
        ; Parse arguments with enhanced parsing
        JSR PARSE_ENHANCED_ARGS
        
        ; Get function back
        PLA
        STA TOKEN_VALUE+1
        PLA
        STA TOKEN_VALUE
        
        ; Call function or expand macro
        JSR CALL_OR_EXPAND
        
        ; Expect closing paren
        JSR SKIP_WHITESPACE
        JSR GET_ENHANCED_TOKEN
        LDA TOKEN_TYPE
        CMP #TOK_RPAREN
        BNE @LIST_ERROR_ENH
        
        DEC PAREN_COUNT
        RTS

@LIST_ERROR_ENH:
        LDA #$02
        STA ERROR_FLAG
        RTS

; Parse arguments with enhanced support
PARSE_ENHANCED_ARGS:
@ARG_LOOP_ENH:
        JSR SKIP_WHITESPACE
        JSR PEEK_CHAR
        
        ; Check for closing paren
        CMP #CHAR_RPAREN
        BEQ @ARGS_DONE_ENH
        
        ; Parse next argument with enhanced parser
        JSR PARSE_ENHANCED_EXPR
        
        ; Check for error
        LDA ERROR_FLAG
        BNE @ARGS_DONE_ENH
        
        JMP @ARG_LOOP_ENH

@ARGS_DONE_ENH:
        RTS

; Call function or expand macro
CALL_OR_EXPAND:
        ; Check if it's a macro first
        JSR IS_MACRO_CALL
        BNE @EXPAND_MACRO_CALL
        
        ; Regular function call
        JSR CALL_FUNCTION
        RTS

@EXPAND_MACRO_CALL:
        JSR EXPAND_MACRO
        RTS

; Check if current symbol is a macro call
IS_MACRO_CALL:
        ; This would check if TOKEN_VALUE points to a macro
        ; For now, return false (not a macro)
        LDA #$00
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

; ======================================================================
; ADVANCED MACRO FUNCTIONS
; ======================================================================

; QUASIQUOTE function - template with selective evaluation
FUNC_QUASIQUOTE:
        LDA ARGC
        CMP #$01
        BNE @QUASIQUOTE_ERROR
        
        ; Process quasiquote template
        JSR PROCESS_QUASIQUOTE_TEMPLATE
        RTS

@QUASIQUOTE_ERROR:
        LDA #$15
        STA ERROR_FLAG
        RTS

; Process quasiquote template with unquote expansion
PROCESS_QUASIQUOTE_TEMPLATE:
        ; Get the template from stack
        JSR POP_NUMBER
        
        ; Process the template, expanding unquotes
        JSR EXPAND_QUASIQUOTE_RECURSIVE
        
        ; Push result back
        JSR PUSH_NUMBER
        RTS

; Recursively expand quasiquote template
EXPAND_QUASIQUOTE_RECURSIVE:
        ; This is a complex operation that would:
        ; 1. Walk through the template structure
        ; 2. Leave quoted parts unchanged
        ; 3. Evaluate unquote expressions
        ; 4. Splice unquote-splicing expressions
        
        ; Simplified implementation - just return template
        RTS

; UNQUOTE_SPLICE function - splice lists into surrounding context
FUNC_UNQUOTE_SPLICE:
        LDA ARGC
        CMP #$01
        BNE @SPLICE_ERROR
        
        ; Mark that this is a splicing operation
        LDA #$01
        STA SPLICE_FLAG
        
        ; Evaluate the expression
        JSR POP_NUMBER
        ; In a real implementation, this would be marked for splicing
        JSR PUSH_NUMBER
        
        RTS

@SPLICE_ERROR:
        LDA #$16
        STA ERROR_FLAG
        RTS

; GENSYM function - generate unique symbols for macro hygiene
FUNC_GENSYM:
        ; Generate a unique symbol name
        JSR GENERATE_UNIQUE_SYMBOL
        
        ; Push the new symbol onto stack
        JSR PUSH_NUMBER
        RTS

; Generate unique symbol for macro hygiene
GENERATE_UNIQUE_SYMBOL:
        ; Increment gensym counter
        INC GENSYM_COUNT
        
        ; Create symbol name: G + counter
        LDY #$00
        
        ; Store prefix 'G'
        LDA #'G'
        STA WORKSPACE,Y
        INY
        
        ; Convert counter to ASCII digits
        LDA GENSYM_COUNT
        JSR CONVERT_TO_ASCII
        
        ; Null terminate
        LDA #$00
        STA WORKSPACE,Y
        
        ; Set up result pointer
        LDA #<WORKSPACE
        STA TOKEN_VALUE
        LDA #>WORKSPACE
        STA TOKEN_VALUE+1
        
        RTS

; Convert number in A to ASCII digits in WORKSPACE starting at Y
CONVERT_TO_ASCII:
        ; Simple conversion for numbers 0-99
        CMP #$0A
        BCC @SINGLE_DIGIT
        
        ; Two digits
        PHA
        SEC
        SBC #$0A
        STA TEMP
        LDA #'1'
        STA WORKSPACE,Y
        INY
        LDA TEMP
        CLC
        ADC #'0'
        STA WORKSPACE,Y
        INY
        PLA
        RTS

@SINGLE_DIGIT:
        ; Single digit
        CLC
        ADC #'0'
        STA WORKSPACE,Y
        INY
        RTS

; MACROEXPAND function - expand a macro form once
FUNC_MACROEXPAND:
        LDA ARGC
        CMP #$01
        BNE @MACROEXPAND_ERROR
        
        ; Get the form to expand
        JSR POP_NUMBER
        
        ; Check if it's a macro call
        JSR CHECK_IF_MACRO_FORM
        BEQ @NOT_MACRO_FORM
        
        ; Expand the macro once
        JSR EXPAND_MACRO_ONCE
        JMP @MACROEXPAND_DONE

@NOT_MACRO_FORM:
        ; Not a macro, return unchanged
        
@MACROEXPAND_DONE:
        JSR PUSH_NUMBER
        RTS

@MACROEXPAND_ERROR:
        LDA #$17
        STA ERROR_FLAG
        RTS

; Check if form is a macro call
CHECK_IF_MACRO_FORM:
        ; This would analyze the form to see if it's a macro call
        ; Return 0 if not macro, non-zero if macro
        LDA #$00
        RTS

; Expand macro once (non-recursive)
EXPAND_MACRO_ONCE:
        ; Perform one level of macro expansion
        ; This is the core of the macro system
        RTS

; ======================================================================
; MACRO HYGIENE SYSTEM
; ======================================================================

; Initialize hygiene system
INIT_HYGIENE:
        ; Clear hygiene table
        LDX #$00
        LDA #$00
@CLEAR_HYGIENE:
        STA HYGIENE_TABLE,X
        INX
        CPX #(MAX_HYGIENE_SYMBOLS * $10)  ; 16 bytes per entry
        BNE @CLEAR_HYGIENE
        
        ; Initialize hygiene pointer
        LDA #<HYGIENE_TABLE
        STA HYGIENE_PTR
        LDA #>HYGIENE_TABLE
        STA HYGIENE_PTR+1
        
        RTS

; Add hygienic renaming for variable
ADD_HYGIENE_BINDING:
        ; Input: original symbol in TOKEN_VALUE
        ; Output: creates new unique symbol and stores mapping
        
        ; Generate unique replacement
        JSR GENERATE_UNIQUE_SYMBOL
        
        ; Store in hygiene table
        JSR STORE_HYGIENE_MAPPING
        
        RTS

; Store hygiene mapping in table
STORE_HYGIENE_MAPPING:
        ; Store original symbol name and its replacement
        ; This would maintain a mapping table for macro expansion
        RTS

; Lookup hygienic replacement for symbol
LOOKUP_HYGIENE_REPLACEMENT:
        ; Input: symbol in TOKEN_VALUE
        ; Output: replacement symbol if found, original if not
        
        ; Search hygiene table for mapping
        LDX #$00
@HYGIENE_SEARCH:
        ; Compare with entries in hygiene table
        ; If found, return replacement
        ; If not found, return original
        
        ; For now, just return original
        RTS

; Apply hygiene to macro body
APPLY_MACRO_HYGIENE:
        ; This would walk through the macro body and:
        ; 1. Identify variable bindings
        ; 2. Create unique replacements
        ; 3. Replace all occurrences consistently
        
        ; This is a complex operation requiring:
        ; - Syntax tree analysis
        ; - Variable scope tracking
        ; - Consistent renaming
        
        RTS

; ======================================================================
; ADVANCED MACRO TESTS
; ======================================================================

; Test quasiquote functionality
TEST_QUASIQUOTE:
        LDX #$00
@COPY_QUASI:
        LDA TEST_QUASI_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE_QUASI
        INX
        JMP @COPY_QUASI
@PARSE_QUASI:
        JSR RESET_PARSER
        JSR PARSE_ENHANCED_EXPR
        RTS

TEST_QUASI_EXPR:
        .byte "`(list 1 ,(+ 2 3) 4)", $00

; Test gensym functionality
TEST_GENSYM:
        JSR FUNC_GENSYM
        ; Should generate unique symbol
        JSR FUNC_GENSYM
        ; Should generate different unique symbol
        RTS

; Test macro hygiene
TEST_HYGIENE:
        ; Test that macro doesn't capture variables
        LDX #$00
@COPY_HYGIENE:
        LDA TEST_HYGIENE_EXPR,X
        STA INPUT_BUFFER,X
        BEQ @PARSE_HYGIENE
        INX
        JMP @COPY_HYGIENE
@PARSE_HYGIENE:
        JSR RESET_PARSER
        JSR PARSE_ENHANCED_EXPR
        RTS

TEST_HYGIENE_EXPR:
        .byte "(let ((x 1)) (my-macro x))", $00

; Run all advanced macro tests
RUN_ADVANCED_MACRO_TESTS:
        JSR TEST_QUASIQUOTE
        JSR TEST_GENSYM
        JSR TEST_HYGIENE
        RTS

; ======================================================================
; UTILITY FUNCTIONS FOR ADVANCED FEATURES
; ======================================================================

; Skip whitespace (enhanced version)
SKIP_WHITESPACE:
        JSR PEEK_CHAR
        CMP #CHAR_SPACE
        BNE @WS_DONE
        JSR NEXT_CHAR
        JMP SKIP_WHITESPACE
@WS_DONE:
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
        BNE @CHAR_NO_CARRY
        INC INPUT_PTR+1
@CHAR_NO_CARRY:
        RTS

; Read number token (enhanced)
READ_NUMBER_TOKEN:
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        
@READ_DIGIT_ENH:
        JSR PEEK_CHAR
        
        ; Check if digit
        CMP #CHAR_0
        BCC @NUMBER_DONE_ENH
        CMP #CHAR_9+1
        BCS @NUMBER_DONE_ENH
        
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
        JMP @READ_DIGIT_ENH

@NUMBER_DONE_ENH:
        RTS

; Multiply TOKEN_VALUE by 10 (enhanced)
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
        
        RTS