#lang racket
(define a 4)
(define b 5)

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define (vart x) ; ***OK, something tricky happening here -> 'swap on 'a' and 'b' will complain about modifying a constant
  (let ()        ; but (vart a) doesn't complain... but it also DOESN't change 'a'
    (set! x 7))) ; but if we change 'set! x' to 'set a', it DOES change it, but no complaints....?

; Racket automatically handles lexical scope... is this refering to HYGIENE?
(let ([tmp 5]
      [other 6])
  (swap tmp other) ; this won't expand as seen below bc 
  (list tmp other)) ; macro will automatically maintain its OWN lexical scope

(let ([tmp 5] ; here, our return value is using the outer 'tmp'
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (list tmp other))

; ***DEFINE-SYNTAX & SYNTAX-RULES***
; struggled w/ lexical scope/constants ->
; had to wrap things in 'let's AND local defines->
; couldn't just call a macro in the REPL ->

(define-syntax bind ; have to follow a '...' in PATTERN with one in TEMPLATE
  (syntax-rules ()  ; as well!
    ((bind a b) (cons a b))
    ((bind a b c) (cons a (cons b c)))
    ((bind a ...) (list a ...))))

(define-syntax rotate
  (syntax-rules ()
    [(rotate a c ...) ; this is the PATTERN to match
     (shift-to (c ... a) (a c ...))])) ; this is the TEMPLATE ->
                                       ; which itself relies on another macro 
(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...)) ; PATTERN to match -> AH! 'from' doesn't match the "rest", it matches first element of rest!
     (let ([tmp from0]) ; TEMPLATE              we don't think of the ... match as being "atomic" or "encapsulated"
       (set! to from) ... ; this expression is duplicated as many times as necessary to use each identifier in 'to' and 'from'
       (set! to0 tmp))])) ; ***THE NUMBER OF IDENTIFIERS IN THE TO and FROM SEQUENCES MUST BE EQUAL OR ERROR
; so we match the pattern in 'rotate' macro -> 
; which expands to syntax that matches the 'shift-to' macro
; the EXPANSION of 'rotate' places second pattern variable at the front of the
; pattern, and the first pattern variable to the end ->
; the ellipses match everything else (here 3 2 1), which is in the middle of the pattern
; so inside 'shift-to' we lay out a pattern where first (used to be second) element matches to 'from0'
; the FIRST IDENTIFIER/PATTERN VARIABLE of the "rest" (ie ...) matches to 'from' (ie 3) and '...' EVERYTHING LEFT (ie 3 2 1 5)
; ('to0' matches to 'a' (ie 5) and 'to' matches to 'c' (ie 4)
; then we EXPAND (going to use concrete example) ->
; we let 'tmp' be 4, we 'set! to from' which means 'to' is now (3 2 1)
; then we 'set! to0 tmp' which makes

(let ((a 5) (b 4) (c 3) (d 2) (e 1))
  (displayln "Testing ellipses...")
  (for ((i (in-range 5)))
    (rotate a b c d e)
    (displayln (list a b c d e))))

; local 'define' s work to for working w/ vars (outside scope considered constants and have trouble modifying)
(define (tester)                                ; though sometimes it seems you can
  (define a 5)
  (define b 4)
  (rotate a b)
  (display (list a b)))

(define-syntax rotate2
  (syntax-rules ()
    [(rotate2 a c ...) 
     (shift-to2 (c ... a) (a c ...))]))

(define-syntax shift-to2
  (syntax-rules ()
    [(shift-to2 (from0 from ...) (to0 to ...))  
     (let ([tmp from0]) 
         (begin
           (display  to)
           (displayln from))...)]))
         
(let ((a 5) (b 4) (c 3) (d 2) (e 1))
  (rotate2 a b c d e))

(define t a)





