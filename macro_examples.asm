; 6502 Lisp Macro System Examples
; Demonstrates the macro capabilities of the enhanced Lisp parser

; ======================================================================
; MACRO EXAMPLES AND TESTS
; ======================================================================

        .org $2000

; Example macro definitions that would be supported

; 1. WHEN macro - conditional execution
; (defmacro when (condition body)
;   (list 'if condition body))
; 
; Usage: (when (> x 0) (print x))
; Expands to: (if (> x 0) (print x))

WHEN_EXAMPLE:
        .byte "(when (> 5 0) 42)", $00

; 2. UNLESS macro - inverse conditional
; (defmacro unless (condition body)
;   (list 'if (list 'not condition) body))
;
; Usage: (unless (= x 0) (/ 10 x))  
; Expands to: (if (not (= x 0)) (/ 10 x))

UNLESS_EXAMPLE:
        .byte "(unless (= 0 1) 42)", $00

; 3. COND macro - multi-way conditional (advanced)
; (defmacro cond (&rest clauses)
;   (if clauses
;       (if (car (car clauses))
;           (car (cdr (car clauses)))
;           (cond (cdr clauses)))))
;
; Usage: (cond ((= x 1) "one") ((= x 2) "two") (t "other"))

COND_EXAMPLE:
        .byte "(cond ((= 2 1) 100) ((= 2 2) 200) (t 300))", $00

; 4. LET macro - local variable binding (advanced)
; (defmacro let (bindings body)
;   ((lambda (vars vals) body) (mapcar 'car bindings) (mapcar 'cadr bindings)))
;
; Usage: (let ((x 10) (y 20)) (+ x y))

LET_EXAMPLE:
        .byte "(let ((x 10) (y 20)) (+ x y))", $00

; 5. DOTIMES macro - iteration (advanced)  
; (defmacro dotimes (var-count-result &rest body)
;   (let ((var (car var-count-result))
;         (count (cadr var-count-result)))
;     `(let ((,var 0))
;        (while (< ,var ,count)
;          ,@body
;          (set! ,var (+ ,var 1))))))
;
; Usage: (dotimes (i 10) (print i))

DOTIMES_EXAMPLE:
        .byte "(dotimes (i 5) (print i))", $00

; ======================================================================
; UTILITY MACROS
; ======================================================================

; INC macro - increment a value
; (defmacro inc (x) (list '+ x 1))
;
; Usage: (inc x) 
; Expands to: (+ x 1)

INC_EXAMPLE:
        .byte "(inc 5)", $00        ; Should expand to (+ 5 1) = 6

; DEC macro - decrement a value  
; (defmacro dec (x) (list '- x 1))
;
; Usage: (dec x)
; Expands to: (- x 1)

DEC_EXAMPLE:
        .byte "(dec 5)", $00        ; Should expand to (- 5 1) = 4

; SQUARE macro - square a number
; (defmacro square (x) (list '* x x))
;
; Usage: (square 4)
; Expands to: (* 4 4)

SQUARE_EXAMPLE:
        .byte "(square 4)", $00     ; Should expand to (* 4 4) = 16

; ======================================================================
; ADVANCED MACRO FEATURES  
; ======================================================================

; Macros with multiple expressions
; (defmacro progn (&rest body)
;   (cons 'begin body))

PROGN_EXAMPLE:
        .byte "(progn (+ 1 2) (+ 3 4))", $00

; Macros with gensym (unique symbol generation)
; (defmacro swap (a b)
;   (let ((temp (gensym)))
;     `(let ((,temp ,a))
;        (set! ,a ,b)
;        (set! ,b ,temp))))

SWAP_EXAMPLE:
        .byte "(swap x y)", $00

; ======================================================================
; MACRO TESTING FRAMEWORK
; ======================================================================

; Function to test macro expansion
TEST_MACRO_EXPANSION:
        LDX #$00                    ; Test number

@TEST_LOOP:
        ; Load test based on X register
        CPX #$01
        BEQ @TEST_WHEN
        CPX #$02
        BEQ @TEST_UNLESS
        CPX #$03
        BEQ @TEST_INC
        CPX #$04
        BEQ @TEST_DEC
        CPX #$05
        BEQ @TEST_SQUARE
        JMP @TEST_DONE

@TEST_WHEN:
        JSR LOAD_WHEN_TEST
        JSR RUN_MACRO_TEST
        JMP @TEST_NEXT

@TEST_UNLESS:
        JSR LOAD_UNLESS_TEST
        JSR RUN_MACRO_TEST
        JMP @TEST_NEXT

@TEST_INC:
        JSR LOAD_INC_TEST
        JSR RUN_MACRO_TEST
        JMP @TEST_NEXT

@TEST_DEC:
        JSR LOAD_DEC_TEST
        JSR RUN_MACRO_TEST
        JMP @TEST_NEXT

@TEST_SQUARE:
        JSR LOAD_SQUARE_TEST
        JSR RUN_MACRO_TEST
        JMP @TEST_NEXT

@TEST_NEXT:
        INX
        JMP @TEST_LOOP

@TEST_DONE:
        RTS

; Load individual test cases
LOAD_WHEN_TEST:
        LDY #$00
        LDX #$00
@COPY1:
        LDA WHEN_EXAMPLE,X
        STA INPUT_BUFFER,Y
        BEQ @DONE1
        INX
        INY
        JMP @COPY1
@DONE1:
        RTS

LOAD_UNLESS_TEST:
        LDY #$00
        LDX #$00
@COPY2:
        LDA UNLESS_EXAMPLE,X
        STA INPUT_BUFFER,Y
        BEQ @DONE2
        INX
        INY
        JMP @COPY2
@DONE2:
        RTS

LOAD_INC_TEST:
        LDY #$00
        LDX #$00
@COPY3:
        LDA INC_EXAMPLE,X
        STA INPUT_BUFFER,Y
        BEQ @DONE3
        INX
        INY
        JMP @COPY3
@DONE3:
        RTS

LOAD_DEC_TEST:
        LDY #$00
        LDX #$00
@COPY4:
        LDA DEC_EXAMPLE,X
        STA INPUT_BUFFER,Y
        BEQ @DONE4
        INX
        INY
        JMP @COPY4
@DONE4:
        RTS

LOAD_SQUARE_TEST:
        LDY #$00
        LDX #$00
@COPY5:
        LDA SQUARE_EXAMPLE,X
        STA INPUT_BUFFER,Y
        BEQ @DONE5
        INX
        INY
        JMP @COPY5
@DONE5:
        RTS

; Run a macro test
RUN_MACRO_TEST:
        ; Reset parser state
        JSR RESET_PARSER
        
        ; Parse expression (which should expand macro)
        JSR PARSE_EXPR
        
        ; Check for errors
        LDA ERROR_FLAG
        BNE @TEST_ERROR
        
        ; Get result
        JSR POP_NUMBER
        ; Result is now in RESULT register
        
        RTS

@TEST_ERROR:
        ; Handle test error
        RTS

; ======================================================================
; MACRO DEFINITION SYNTAX EXAMPLES
; ======================================================================

; Example of how macros would be defined in source code:

; Simple replacement macro
SIMPLE_MACRO_DEF:
        .byte "(defmacro nil () 0)", $00

; Macro with parameters
PARAM_MACRO_DEF:  
        .byte "(defmacro abs (x) (if (< x 0) (- x) x))", $00

; Macro with multiple parameters
MULTI_PARAM_DEF:
        .byte "(defmacro max (a b) (if (> a b) a b))", $00

; Macro with variable arguments (would need special syntax)
VARARGS_MACRO_DEF:
        .byte "(defmacro list (&rest args) (cons 'quote (cons args nil)))", $00

; ======================================================================
; MACRO UTILITY FUNCTIONS
; ======================================================================

; Check if symbol is a macro
IS_MACRO:
        ; Input: symbol name in TOKEN_VALUE
        ; Output: non-zero in A if macro, zero if not
        JSR LOOKUP_MACRO
        LDA TOKEN_VALUE
        ORA TOKEN_VALUE+1
        RTS

; Expand all macros in an expression
EXPAND_ALL_MACROS:
        ; This would recursively expand all macros in an expression
        ; Until no more macros can be expanded
        RTS

; Get macro parameter count
GET_MACRO_PARAM_COUNT:
        ; Input: macro entry pointer in TOKEN_VALUE
        ; Output: parameter count in A
        LDY #$10    ; Offset to parameter count
        LDA (TOKEN_VALUE),Y
        RTS

; ======================================================================
; ERROR HANDLING FOR MACROS
; ======================================================================

MACRO_ERROR_MESSAGES:
        .byte "Macro recursion too deep", $00
        .byte "Wrong number of macro arguments", $00  
        .byte "Macro expansion failed", $00
        .byte "Invalid macro definition", $00
        .byte "Macro not found", $00

; Print macro error (if output system available)
PRINT_MACRO_ERROR:
        ; This would display appropriate error message
        ; based on ERROR_FLAG value
        RTS