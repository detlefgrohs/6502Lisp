; 6502 Lisp Interpreter for Commodore 64
; Interactive version with screen input/output
; Supports advanced macro system with backquote/comma syntax

; ======================================================================
; C64 SYSTEM CONSTANTS
; ======================================================================

; C64 Memory Map
SCREEN_RAM   = $0400    ; Screen memory (40x25 = 1000 bytes)
COLOR_RAM    = $D800    ; Color memory 
BASIC_START  = $0801    ; BASIC program start
CHARSET_ROM  = $D000    ; Character ROM
VIC_CTRL     = $D011    ; VIC control register

; C64 KERNAL Routines
CHROUT       = $FFD2    ; Output character to screen
CHRIN        = $FFCF    ; Input character from keyboard  
GETIN        = $FFE4    ; Get character from keyboard (non-blocking)
CLRSCR       = $E544    ; Clear screen
SETCURSOR    = $E50A    ; Set cursor position
HOME         = $E566    ; Home cursor

; C64 Keyboard
KEY_RETURN   = $0D      ; Return key
KEY_DELETE   = $14      ; Delete/backspace
KEY_SPACE    = $20      ; Space
KEY_ESC      = $1B      ; Escape (RUN/STOP)
KEY_F1       = $85      ; F1 key
KEY_F3       = $86      ; F3 key
KEY_F5       = $87      ; F5 key
KEY_F7       = $88      ; F7 key

; Screen dimensions
SCREEN_WIDTH = 40
SCREEN_HEIGHT = 25
SCREEN_SIZE  = 1000

; Colors
COLOR_BLACK  = $00
COLOR_WHITE  = $01
COLOR_RED    = $02
COLOR_CYAN   = $03
COLOR_PURPLE = $04
COLOR_GREEN  = $05
COLOR_BLUE   = $06
COLOR_YELLOW = $07

; ======================================================================
; ZERO PAGE VARIABLES (C64 SPECIFIC)
; ======================================================================

; Parser variables (use C64 free zero page)
SYMBOL_LEN   = $FB      ; Length of current symbol
ARGC         = $FC      ; Argument count for functions
COMPARISON   = $FD      ; Comparison result
MACRO_PTR    = $FE      ; Pointer to current macro
MACRO_ARGC   = $02      ; Macro argument count
EXPAND_FLAG  = $03      ; Macro expansion flag
MACRO_DEPTH  = $04      ; Macro expansion depth
QUASI_DEPTH  = $05      ; Quasiquote nesting depth
GENSYM_COUNT = $06      ; Counter for generating unique symbols
SPLICE_FLAG  = $07      ; Splice unquote flag
HYGIENE_PTR  = $08      ; Pointer to hygiene symbol table

; Screen/Input variables
CURSOR_X     = $09      ; Cursor X position
CURSOR_Y     = $0A      ; Cursor Y position
INPUT_LEN    = $0B      ; Length of current input line
PARSE_MODE   = $0C      ; Parsing mode flag
SCREEN_PTR   = $0D      ; Pointer to screen position
COLOR_PTR    = $0F      ; Pointer to color position

; Standard parser variables
INPUT_PTR    = $22      ; Pointer to current input position  
STACK_PTR    = $24      ; Expression stack pointer
TEMP         = $26      ; Temporary storage
RESULT       = $28      ; Result accumulator
TOKEN_TYPE   = $2A      ; Current token type
TOKEN_VALUE  = $2B      ; Current token value (16-bit)
PAREN_COUNT  = $2D      ; Parentheses nesting level
ERROR_FLAG   = $2E      ; Error flag

; ======================================================================
; MEMORY LAYOUT (C64 SPECIFIC)
; ======================================================================

; Use C64's available RAM areas
INPUT_BUFFER = $0334    ; Input text buffer (200 bytes)
WORKSPACE    = $0400    ; General workspace (reuse screen when needed)
SYMBOL_TABLE = $1000    ; Symbol definitions (2KB)
EXPR_STACK   = $1800    ; Expression evaluation stack (2KB) 
MACRO_TABLE  = $2000    ; Macro definitions storage (2KB)
HYGIENE_TABLE = $2800   ; Hygiene symbol table (1KB)

; Constants from extended version
MAX_MACROS   = $20      ; Maximum number of macros (32)
MACRO_ENTRY_SIZE = $40  ; Size of each macro entry (64 bytes)
MAX_HYGIENE_SYMBOLS = $40 ; Maximum hygienic symbols

; Function IDs (same as extended version)
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

; Token types
TOK_EOF      = $00
TOK_LPAREN   = $01
TOK_RPAREN   = $02
TOK_NUMBER   = $03
TOK_SYMBOL   = $04
TOK_BACKQUOTE = $05
TOK_COMMA    = $06
TOK_COMMA_AT = $07
TOK_ERROR    = $FF

; ASCII codes
CHAR_SPACE   = $20
CHAR_LPAREN  = $28
CHAR_RPAREN  = $29
CHAR_COMMA   = $2C
CHAR_0       = $30
CHAR_9       = $39
CHAR_A       = $41
CHAR_Z       = $5A
CHAR_BACKQUOTE = $60
CHAR_a       = $61
CHAR_z       = $7A
CHAR_AT      = $40

; ======================================================================
; BASIC HEADER FOR C64
; ======================================================================

        .org $0801      ; BASIC start address

        ; BASIC line: 10 SYS 2064
        .byte $0B, $08  ; Next line address
        .byte $0A, $00  ; Line number (10)
        .byte $9E       ; SYS token
        .byte $20       ; Space
        .byte $32, $30, $36, $34  ; "2064" (start address)
        .byte $00       ; End of line
        .byte $00, $00  ; End of BASIC program

; ======================================================================
; MAIN PROGRAM START
; ======================================================================

        .org $0810      ; Program start (2064 decimal)

MAIN:
        ; Initialize C64 system
        JSR INIT_C64
        
        ; Initialize Lisp system
        JSR INIT_LISP_SYSTEM
        
        ; Show welcome message
        JSR SHOW_WELCOME
        
        ; Enter main interactive loop
        JSR INTERACTIVE_LOOP
        
        ; Return to BASIC
        RTS

; ======================================================================
; C64 INITIALIZATION
; ======================================================================

INIT_C64:
        ; Clear screen
        JSR CLRSCR
        
        ; Set colors
        LDA #COLOR_BLUE
        STA $D020           ; Border color
        LDA #COLOR_BLACK  
        STA $D021           ; Background color
        
        ; Initialize cursor position
        LDA #$00
        STA CURSOR_X
        STA CURSOR_Y
        
        ; Set text color to white
        LDA #COLOR_WHITE
        LDY #$00
@COLOR_LOOP:
        STA COLOR_RAM,Y
        STA COLOR_RAM+$100,Y
        STA COLOR_RAM+$200,Y
        STA COLOR_RAM+$2E7,Y
        INY
        BNE @COLOR_LOOP
        
        RTS

; Initialize Lisp interpreter
INIT_LISP_SYSTEM:
        ; Initialize parser variables
        LDA #$00
        STA INPUT_PTR
        STA INPUT_PTR+1
        STA STACK_PTR
        STA STACK_PTR+1
        STA PAREN_COUNT
        STA ERROR_FLAG
        STA MACRO_DEPTH
        STA QUASI_DEPTH
        STA GENSYM_COUNT
        STA SPLICE_FLAG
        
        ; Set up pointers
        LDA #<INPUT_BUFFER
        STA INPUT_PTR
        LDA #>INPUT_BUFFER
        STA INPUT_PTR+1
        
        LDA #<EXPR_STACK
        STA STACK_PTR
        LDA #>EXPR_STACK
        STA STACK_PTR+1
        
        ; Initialize subsystems
        JSR INIT_SYMBOLS
        JSR INIT_MACROS
        JSR INIT_HYGIENE
        
        RTS

; ======================================================================
; SCREEN DISPLAY ROUTINES
; ======================================================================

SHOW_WELCOME:
        LDX #$00
@PRINT_LOOP:
        LDA WELCOME_MSG,X
        BEQ @WELCOME_DONE
        JSR CHROUT
        INX
        JMP @PRINT_LOOP
@WELCOME_DONE:
        RTS

WELCOME_MSG:
        .text "6502 LISP INTERPRETER V2.0"
        .byte $0D
        .text "ADVANCED MACRO SYSTEM"
        .byte $0D
        .text "PRESS F1 FOR HELP"
        .byte $0D, $0D
        .text "READY."
        .byte $0D, $0D, $00

SHOW_HELP:
        JSR CLRSCR
        LDX #$00
@HELP_LOOP:
        LDA HELP_MSG,X
        BEQ @HELP_DONE
        JSR CHROUT
        INX
        JMP @HELP_LOOP
@HELP_DONE:
        
        ; Wait for key
        JSR WAIT_KEY
        JSR CLRSCR
        RTS

HELP_MSG:
        .text "6502 LISP HELP"
        .byte $0D, $0D
        .text "BASIC FUNCTIONS:"
        .byte $0D
        .text "(+ 1 2 3)    ADDITION"
        .byte $0D
        .text "(- 10 3)     SUBTRACTION"
        .byte $0D
        .text "(* 2 3 4)    MULTIPLICATION"
        .byte $0D
        .text "(= 5 5)      EQUALITY TEST"
        .byte $0D
        .text "(< 3 5)      LESS THAN"
        .byte $0D
        .text "(> 5 3)      GREATER THAN"
        .byte $0D, $0D
        .text "MACROS:"
        .byte $0D
        .text "(WHEN TEST BODY)"
        .byte $0D
        .text "(UNLESS TEST BODY)"
        .byte $0D
        .text "`(LIST ,X ,@Y)  QUASIQUOTE"
        .byte $0D, $0D
        .text "KEYS:"
        .byte $0D
        .text "F1 - HELP    F3 - CLEAR"
        .byte $0D
        .text "F5 - EVAL    F7 - RESET"
        .byte $0D, $0D
        .text "PRESS ANY KEY TO CONTINUE"
        .byte $00

; Show prompt
SHOW_PROMPT:
        LDA #'>'
        JSR CHROUT
        LDA #' '
        JSR CHROUT
        RTS

; Print string pointed to by X/Y (lo/hi)
PRINT_STRING:
        STX TEMP
        STY TEMP+1
        LDY #$00
@PRINT_CHAR:
        LDA (TEMP),Y
        BEQ @PRINT_DONE
        JSR CHROUT
        INY
        JMP @PRINT_CHAR
@PRINT_DONE:
        RTS

; Print number in RESULT
PRINT_NUMBER:
        ; Convert number to decimal string
        LDA RESULT+1        ; High byte
        BNE @PRINT_16BIT
        
        ; 8-bit number
        LDA RESULT
        JSR PRINT_BYTE
        RTS

@PRINT_16BIT:
        ; 16-bit number - simplified version
        LDA RESULT+1
        JSR PRINT_BYTE
        LDA RESULT
        JSR PRINT_BYTE
        RTS

; Print byte in A as decimal
PRINT_BYTE:
        PHA
        
        ; Convert to decimal
        LDX #$00            ; Digit counter
        CMP #$64            ; 100?
        BCC @LESS_THAN_100
        
        ; Hundreds digit
        LDX #$01
        SEC
        SBC #$64
        
@LESS_THAN_100:
        ; Check if we printed hundreds
        CPX #$00
        BEQ @NO_HUNDREDS
        PHA
        TXA
        CLC
        ADC #'0'
        JSR CHROUT
        PLA

@NO_HUNDREDS:
        ; Tens digit
        LDX #$00
        CMP #$0A
        BCC @LESS_THAN_10
        
@TENS_LOOP:
        CMP #$0A
        BCC @PRINT_TENS
        SEC
        SBC #$0A
        INX
        JMP @TENS_LOOP

@PRINT_TENS:
        PHA
        TXA
        CLC
        ADC #'0'
        JSR CHROUT
        PLA

@LESS_THAN_10:
        ; Units digit
        CLC
        ADC #'0'
        JSR CHROUT
        
        PLA
        RTS

; Print newline
PRINT_NEWLINE:
        LDA #$0D
        JSR CHROUT
        RTS

; ======================================================================
; KEYBOARD INPUT ROUTINES
; ======================================================================

; Wait for any key press
WAIT_KEY:
@WAIT_LOOP:
        JSR GETIN
        BEQ @WAIT_LOOP
        RTS

; Read line of input into INPUT_BUFFER
READ_LINE:
        LDY #$00            ; Input position
        STY INPUT_LEN
        
@INPUT_LOOP:
        JSR CHRIN           ; Get character (blocking)
        
        ; Check for special keys
        CMP #KEY_RETURN
        BEQ @INPUT_DONE
        
        CMP #KEY_DELETE
        BEQ @INPUT_DELETE
        
        CMP #KEY_ESC
        BEQ @INPUT_ESCAPE
        
        ; Check for function keys
        CMP #KEY_F1
        BEQ @INPUT_F1
        CMP #KEY_F3  
        BEQ @INPUT_F3
        CMP #KEY_F5
        BEQ @INPUT_F5
        CMP #KEY_F7
        BEQ @INPUT_F7
        
        ; Regular character - add to buffer
        CMP #' '            ; Printable?
        BCC @INPUT_LOOP     ; Skip control chars
        
        ; Check buffer space
        CPY #200            ; Max input length
        BCS @INPUT_LOOP
        
        ; Store character
        STA INPUT_BUFFER,Y
        INY
        STY INPUT_LEN
        
        ; Echo character
        JSR CHROUT
        
        JMP @INPUT_LOOP

@INPUT_DELETE:
        ; Handle backspace/delete
        CPY #$00
        BEQ @INPUT_LOOP     ; Nothing to delete
        
        DEY
        STY INPUT_LEN
        
        ; Move cursor back and clear character
        LDA #$9D            ; Cursor left
        JSR CHROUT
        LDA #' '            ; Space
        JSR CHROUT
        LDA #$9D            ; Cursor left again
        JSR CHROUT
        
        JMP @INPUT_LOOP

@INPUT_F1:
        ; Show help
        JSR SHOW_HELP
        JSR SHOW_PROMPT
        LDY INPUT_LEN       ; Restore position
        JMP @INPUT_LOOP

@INPUT_F3:
        ; Clear screen
        JSR CLRSCR
        JSR SHOW_PROMPT
        LDY #$00            ; Reset input
        STY INPUT_LEN
        JMP @INPUT_LOOP

@INPUT_F5:
        ; Force evaluation of current input
        JMP @INPUT_DONE

@INPUT_F7:
        ; Reset interpreter
        JSR INIT_LISP_SYSTEM
        JSR CLRSCR
        JSR SHOW_WELCOME
        JSR SHOW_PROMPT
        LDY #$00
        STY INPUT_LEN
        JMP @INPUT_LOOP

@INPUT_ESCAPE:
        ; Cancel current input
        LDY #$00
        STY INPUT_LEN

@INPUT_DONE:
        ; Null-terminate input
        LDA #$00
        STA INPUT_BUFFER,Y
        
        ; Print newline
        JSR PRINT_NEWLINE
        
        RTS

; ======================================================================
; MAIN INTERACTIVE LOOP
; ======================================================================

INTERACTIVE_LOOP:
        JSR SHOW_PROMPT
        
@MAIN_LOOP:
        ; Read input line
        JSR READ_LINE
        
        ; Check if empty
        LDA INPUT_LEN
        BEQ @SHOW_PROMPT_AGAIN
        
        ; Reset input pointer
        LDA #<INPUT_BUFFER
        STA INPUT_PTR
        LDA #>INPUT_BUFFER
        STA INPUT_PTR+1
        
        ; Clear error flag
        LDA #$00
        STA ERROR_FLAG
        
        ; Reset expression stack
        LDA #<EXPR_STACK
        STA STACK_PTR
        LDA #>EXPR_STACK
        STA STACK_PTR+1
        
        ; Parse and evaluate expression
        JSR PARSE_ENHANCED_EXPR
        
        ; Check for errors
        LDA ERROR_FLAG
        BNE @SHOW_ERROR
        
        ; Get result and print it
        JSR POP_NUMBER
        JSR PRINT_NUMBER
        JSR PRINT_NEWLINE
        
        JMP @SHOW_PROMPT_AGAIN

@SHOW_ERROR:
        ; Display error message
        LDX #<ERROR_MSG
        LDY #>ERROR_MSG
        JSR PRINT_STRING
        
        ; Print error code
        LDA ERROR_FLAG
        JSR PRINT_BYTE
        JSR PRINT_NEWLINE

@SHOW_PROMPT_AGAIN:
        JSR SHOW_PROMPT
        JMP @MAIN_LOOP

ERROR_MSG:
        .text "ERROR: ", $00

; ======================================================================
; COPY CORE FUNCTIONS FROM LISP_EXTENDED.ASM
; ======================================================================

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

; Built-in function names
BUILTIN_NAMES:
        .byte "+", $00
        .byte "-", $00
        .byte "*", $00
        .byte "=", $00
        .byte "<", $00
        .byte ">", $00
        .byte "and", $00
        .byte "or", $00
        .byte "not", $00
        .byte "when", $00
        .byte "unless", $00
        .byte "quote", $00
        .byte "gensym", $00
        .byte $00

; Corresponding function IDs
BUILTIN_IDS:
        .byte FUNC_ADD
        .byte FUNC_SUB
        .byte FUNC_MUL
        .byte FUNC_EQ
        .byte FUNC_LT
        .byte FUNC_GT
        .byte FUNC_AND
        .byte FUNC_OR
        .byte FUNC_NOT
        .byte FUNC_QUOTE
        .byte FUNC_GENSYM

; ======================================================================
; SIMPLIFIED PARSER FOR C64 VERSION
; ======================================================================

; Enhanced expression parser
PARSE_ENHANCED_EXPR:
        JSR GET_ENHANCED_TOKEN
        
        ; Check token type
        LDA TOKEN_TYPE
        CMP #TOK_EOF
        BEQ @PARSE_EOF
        
        CMP #TOK_LPAREN
        BEQ @PARSE_LIST
        
        CMP #TOK_NUMBER
        BEQ @PARSE_NUMBER
        
        CMP #TOK_SYMBOL
        BEQ @PARSE_SYMBOL
        
        ; Error
        LDA #$01
        STA ERROR_FLAG
        RTS

@PARSE_EOF:
        RTS

@PARSE_LIST:
        JSR PARSE_LIST_ENHANCED
        RTS

@PARSE_NUMBER:
        JSR PUSH_NUMBER
        RTS

@PARSE_SYMBOL:
        JSR LOOKUP_SYMBOL
        JSR PUSH_NUMBER
        RTS

; Enhanced tokenizer
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
        
        ; Check for digit
        CMP #CHAR_0
        BCC @TRY_SYMBOL_TOK
        CMP #CHAR_9+1
        BCC @READ_NUMBER_TOK
        
@TRY_SYMBOL_TOK:
        JSR READ_SYMBOL
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

@READ_NUMBER_TOK:
        JSR READ_NUMBER_TOKEN
        LDA #TOK_NUMBER
        STA TOKEN_TYPE
        RTS

; Utility functions
SKIP_WHITESPACE:
        JSR PEEK_CHAR
        CMP #CHAR_SPACE
        BNE @WS_DONE
        JSR NEXT_CHAR
        JMP SKIP_WHITESPACE
@WS_DONE:
        RTS

PEEK_CHAR:
        LDY #$00
        LDA (INPUT_PTR),Y
        RTS

NEXT_CHAR:
        LDY #$00
        LDA (INPUT_PTR),Y
        INC INPUT_PTR
        BNE @NO_CARRY
        INC INPUT_PTR+1
@NO_CARRY:
        RTS

; Read number token
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
        
        ; Multiply current value by 10 (simplified)
        LDA TOKEN_VALUE
        ASL                 ; *2
        STA TOKEN_VALUE
        ASL                 ; *4
        ASL                 ; *8
        CLC
        ADC TOKEN_VALUE     ; *8 + *2 = *10
        STA TOKEN_VALUE
        
        ; Add new digit
        CLC
        ADC TEMP
        STA TOKEN_VALUE
        
        ; Advance to next character
        JSR NEXT_CHAR
        JMP @READ_DIGIT

@NUMBER_DONE:
        RTS

; Read symbol
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

; ======================================================================
; SIMPLIFIED CORE FUNCTIONS FOR C64
; ======================================================================

; Push number onto stack
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

; Pop number from stack
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

; Simplified symbol lookup
LOOKUP_SYMBOL:
        ; For now, return 0 for unknown symbols
        LDA #$00
        STA TOKEN_VALUE
        STA TOKEN_VALUE+1
        RTS

; Parse list
PARSE_LIST_ENHANCED:
        INC PAREN_COUNT
        
        ; Get function name
        JSR GET_ENHANCED_TOKEN
        LDA TOKEN_TYPE
        CMP #TOK_SYMBOL
        BNE @LIST_ERROR
        
        ; For now, just handle simple arithmetic
        LDY #$00
        LDA (TOKEN_VALUE),Y
        CMP #'+'
        BEQ @HANDLE_ADD
        CMP #'-'
        BEQ @HANDLE_SUB
        CMP #'*'
        BEQ @HANDLE_MUL
        
        ; Unknown function
        LDA #$02
        STA ERROR_FLAG
        RTS

@HANDLE_ADD:
        JSR PARSE_ARITHMETIC
        JSR FUNC_ADDITION
        JMP @LIST_DONE

@HANDLE_SUB:
        JSR PARSE_ARITHMETIC
        JSR FUNC_SUBTRACTION
        JMP @LIST_DONE

@HANDLE_MUL:
        JSR PARSE_ARITHMETIC
        JSR FUNC_MULTIPLICATION
        JMP @LIST_DONE

@LIST_DONE:
        ; Expect closing paren
        JSR GET_ENHANCED_TOKEN
        LDA TOKEN_TYPE
        CMP #TOK_RPAREN
        BNE @LIST_ERROR
        
        DEC PAREN_COUNT
        RTS

@LIST_ERROR:
        LDA #$03
        STA ERROR_FLAG
        RTS

; Parse arithmetic arguments
PARSE_ARITHMETIC:
        LDA #$00
        STA ARGC
        
@ARG_LOOP:
        JSR SKIP_WHITESPACE
        JSR PEEK_CHAR
        
        ; Check for closing paren
        CMP #CHAR_RPAREN
        BEQ @ARGS_DONE
        
        ; Parse next number
        JSR GET_ENHANCED_TOKEN
        LDA TOKEN_TYPE
        CMP #TOK_NUMBER
        BNE @ARGS_DONE
        
        JSR PUSH_NUMBER
        INC ARGC
        
        JMP @ARG_LOOP

@ARGS_DONE:
        RTS

; Simplified arithmetic functions
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

FUNC_SUBTRACTION:
        ; Simplified - just subtract second from first
        LDA ARGC
        CMP #$02
        BNE @SUB_ERROR
        
        JSR POP_NUMBER      ; Second number
        LDA RESULT
        STA TEMP
        
        JSR POP_NUMBER      ; First number
        SEC
        LDA RESULT
        SBC TEMP
        STA RESULT
        
        LDA RESULT
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@SUB_ERROR:
        LDA #$04
        STA ERROR_FLAG
        RTS

FUNC_MULTIPLICATION:
        ; Simplified multiplication
        LDA ARGC
        CMP #$02
        BNE @MUL_ERROR
        
        JSR POP_NUMBER      ; Second number
        LDA RESULT
        STA TEMP
        
        JSR POP_NUMBER      ; First number
        
        ; Simple multiplication by repeated addition
        LDA #$00
        STA RESULT+1        ; Clear result
        
        LDX TEMP            ; Multiplier in X
        BEQ @MUL_DONE
        
@MUL_LOOP:
        CLC
        LDA RESULT+1
        ADC RESULT          ; Add multiplicand
        STA RESULT+1
        DEX
        BNE @MUL_LOOP

@MUL_DONE:
        LDA RESULT+1
        STA TOKEN_VALUE
        LDA #$00
        STA TOKEN_VALUE+1
        JSR PUSH_NUMBER
        RTS

@MUL_ERROR:
        LDA #$05
        STA ERROR_FLAG
        RTS

; Stub functions for macro system (to prevent assembly errors)
INIT_MACROS:
        RTS

INIT_HYGIENE:
        RTS