; Example Lisp expressions for the 6502 Lisp Parser
; These can be loaded into the INPUT_BUFFER for testing

; Basic arithmetic examples
EXAMPLE_1:
    .byte "(+ 1 2 3)", $00           ; Addition: 1 + 2 + 3 = 6

EXAMPLE_2:  
    .byte "(- 20 5 3)", $00          ; Subtraction: 20 - 5 - 3 = 12

EXAMPLE_3:
    .byte "(* 2 3 4)", $00           ; Multiplication: 2 * 3 * 4 = 24

; Comparison examples
EXAMPLE_4:
    .byte "(= 10 10)", $00           ; Equality: 10 == 10 = true (1)

EXAMPLE_5:
    .byte "(< 5 10)", $00            ; Less than: 5 < 10 = true (1)

EXAMPLE_6:
    .byte "(> 15 10)", $00           ; Greater than: 15 > 10 = true (1)

; Logical operation examples  
EXAMPLE_7:
    .byte "(and 1 1 1)", $00         ; Logical AND: all true = true (1)

EXAMPLE_8:
    .byte "(or 0 0 1)", $00          ; Logical OR: at least one true = true (1)

EXAMPLE_9:
    .byte "(not 0)", $00             ; Logical NOT: not false = true (1)

; Nested expression examples
EXAMPLE_10:
    .byte "(+ 1 (* 2 3))", $00       ; Nested: 1 + (2 * 3) = 1 + 6 = 7

EXAMPLE_11:
    .byte "(and (= 5 5) (< 3 10))", $00  ; Complex: (5==5) AND (3<10) = true AND true = true (1)

EXAMPLE_12:
    .byte "(+ (* 2 3) (- 10 5))", $00    ; Multiple nested: (2*3) + (10-5) = 6 + 5 = 11

; Edge cases
EXAMPLE_13:
    .byte "(+ 0)", $00               ; Single argument addition = 0

EXAMPLE_14:
    .byte "(- 5)", $00               ; Single argument subtraction = -5

EXAMPLE_15:
    .byte "(and)", $00               ; No arguments AND = true (1)

EXAMPLE_16:
    .byte "(or)", $00                ; No arguments OR = false (0)

; Error cases (these should set ERROR_FLAG)
ERROR_EXAMPLE_1:
    .byte "(unknown 1 2)", $00       ; Unknown function

ERROR_EXAMPLE_2:
    .byte "(+ 1 2", $00              ; Missing closing parenthesis

ERROR_EXAMPLE_3:
    .byte "+ 1 2)", $00              ; Missing opening parenthesis

; Complex nested expression
COMPLEX_EXAMPLE:
    .byte "(+ (* 2 (+ 3 4)) (- 20 (* 3 5)))", $00
    ; Breakdown:
    ; Inner: (+ 3 4) = 7
    ; Inner: (* 2 7) = 14  
    ; Inner: (* 3 5) = 15
    ; Inner: (- 20 15) = 5
    ; Final: (+ 14 5) = 19

; Function to load a specific example into INPUT_BUFFER
; Call with example number in A register (1-16)
LOAD_EXAMPLE:
    CMP #$01
    BEQ @LOAD_EX1
    CMP #$02
    BEQ @LOAD_EX2
    CMP #$03
    BEQ @LOAD_EX3
    CMP #$04
    BEQ @LOAD_EX4
    CMP #$05
    BEQ @LOAD_EX5
    CMP #$06
    BEQ @LOAD_EX6
    CMP #$07
    BEQ @LOAD_EX7
    CMP #$08
    BEQ @LOAD_EX8
    CMP #$09
    BEQ @LOAD_EX9
    CMP #$0A
    BEQ @LOAD_EX10
    CMP #$0B
    BEQ @LOAD_EX11
    CMP #$0C
    BEQ @LOAD_EX12
    ; Add more as needed...
    RTS

@LOAD_EX1:
    LDX #<EXAMPLE_1
    LDY #>EXAMPLE_1
    JMP @COPY_EXAMPLE

@LOAD_EX2:
    LDX #<EXAMPLE_2  
    LDY #>EXAMPLE_2
    JMP @COPY_EXAMPLE

@LOAD_EX3:
    LDX #<EXAMPLE_3
    LDY #>EXAMPLE_3
    JMP @COPY_EXAMPLE

@LOAD_EX4:
    LDX #<EXAMPLE_4
    LDY #>EXAMPLE_4
    JMP @COPY_EXAMPLE

@LOAD_EX5:
    LDX #<EXAMPLE_5
    LDY #>EXAMPLE_5
    JMP @COPY_EXAMPLE

@LOAD_EX6:
    LDX #<EXAMPLE_6
    LDY #>EXAMPLE_6
    JMP @COPY_EXAMPLE

@LOAD_EX7:
    LDX #<EXAMPLE_7
    LDY #>EXAMPLE_7
    JMP @COPY_EXAMPLE

@LOAD_EX8:
    LDX #<EXAMPLE_8
    LDY #>EXAMPLE_8
    JMP @COPY_EXAMPLE

@LOAD_EX9:
    LDX #<EXAMPLE_9
    LDY #>EXAMPLE_9
    JMP @COPY_EXAMPLE

@LOAD_EX10:
    LDX #<EXAMPLE_10
    LDY #>EXAMPLE_10
    JMP @COPY_EXAMPLE

@LOAD_EX11:
    LDX #<EXAMPLE_11
    LDY #>EXAMPLE_11
    JMP @COPY_EXAMPLE

@LOAD_EX12:
    LDX #<EXAMPLE_12
    LDY #>EXAMPLE_12
    JMP @COPY_EXAMPLE

; Copy example from X,Y pointer to INPUT_BUFFER
@COPY_EXAMPLE:
    STX TEMP
    STY TEMP+1
    
    LDY #$00
    LDX #$00
@COPY_LOOP:
    LDA (TEMP),Y
    STA INPUT_BUFFER,X
    BEQ @COPY_DONE
    INY
    INX
    JMP @COPY_LOOP

@COPY_DONE:
    RTS

; Test runner that runs through all examples
RUN_ALL_EXAMPLES:
    LDX #$01                    ; Start with example 1

@TEST_LOOP:
    TXA
    JSR LOAD_EXAMPLE            ; Load example X
    JSR RESET_PARSER            ; Reset parser state  
    JSR PARSE_EXPR              ; Parse expression
    
    ; Check for errors
    LDA ERROR_FLAG
    BNE @TEST_ERROR
    
    ; Get result
    JSR POP_NUMBER              ; Result now in RESULT
    
    ; Store result for inspection
    ; (In a real system, you might output this)
    
@TEST_CONTINUE:
    INX
    CPX #$0D                    ; Test examples 1-12
    BCC @TEST_LOOP
    RTS

@TEST_ERROR:
    ; Handle test error
    ; (In a real system, you might log this)
    JMP @TEST_CONTINUE