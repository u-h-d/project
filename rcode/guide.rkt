#lang racket

; expression ->
(+ 1 2)

; nested expression ->
(+ 1 (- 3 2))

; binding ->
(define a 10)
(define b 15)

; bindings shadowed by 'new-procedure'
(define x 7)
(define y 8)

; compound procedure ->
(define (new-procedure x y)
  (displayln x)
  (displayln y))

(new-procedure a b)

x
y

; 'cons' primitive operator ->

(cons x y)



; characters ->

(char? #\A) ; scalar value w/ #\ in front -> evaluator knows how to handle then

(displayln #\A)

(char->integer #\A)

; symbols ->
; this is NOT a symbol ->
(symbol? a) ; #f bc unquoted it is an IDENTIFIER
(symbol? 'a) ; #t bc when we quote it we are thinking of it ONLY as that arbitrary mark

; strings ->

; vectors ->


(display "\nVECTORS ->\n")

(vector? '#("a" "b" "c"))

; functions
; really just another value/expression
; safest way to use functions is to take in args, and return a value -> don't change anything
; what can a procedure take for parameters? (is a procedure a value? YES)

; list building procedure ->
; 'lambda' expression creates function
(define builder
  (lambda (x l)
    (cons x l)))

(define l
  '(#\a #\b #\c))
(define l2
  '(a b c))

; internal definitions ->
; SCOPE/SHADOWING ->
(define t
  4)

(define (t-func t)
  (define (inner-func t) ; inside this environment, t is whatever we DESIGNATE
    t)
  (values
   t
   (inner-func (- t 1))))

(define z 2) ; global x represents the value of 2

(define (outer-inner z) ; in here, 'z' will be whatever we give it, not what it is on the outside...
  (define (core y)
    ; we can access 'z' in here bc we are enclosed by the 'outer-inner' environment
    ; no need for 'core' take 'z' and pass it around, we don't OPERATE on it
    (if (< y z) 
        y
        (core (- y 1))))
  (core (* z z z)))

; 'let' vs 'define'
  
; simple constants are expressions that evaluate to themselves -> don't need parens...   


; have to do well in explaining lists/pairs
; probably need to get into "grammars", bc integral to understanding abstractions/representations
; gonna have to have sections for Linux/Bash...?

; side effects ->
; when an operator manipulates something outside its given parameters ->
; i/o IS a side effect, but a very useful one
(printf "\nHello ~a" (car l))

; functional representation of a mine that yields characters...
; do we need an ACTUAL pile of chars to be the mine? NO, but maybe we'd like that at some point
(define (mine)
  (integer->char (random 97 123)))

(define (display-mine)
  (printf "\n~a" (mine)))

(for ((i 100))
  (printf "\n~a" (mine)))

; a DATA representation of a mine could be a long string, a LIST of chars, an ARRAY of chars

; might be good chance to show that the REPRESENTATION of a mine can be dealt with
; inside 'miner' (ie, can handle different representations)

(define gem #\z)
; the miner searches for rare "gems" (chars)
; then returns them to stockpile
(define (miner m gem)
  (define (mining)
    ; 'm' is a mine of some type
    (if (char=? (m) gem)
        ; good lesson here... had 'print' in tail position initially... screwed things up
        ; bc the value of the 'begin' was void...
        (begin
          (printf "\nGem of type '~a' is found!" gem)
          gem) ; had this as #t, but actually need to return the char when found
        (begin
          ;(sleep 0.1)
          (displayln "Mining...")
          (mining))))
  (mining))

(define (go-mine)
  (miner mine gem))

; 
; need to keep track of gems -> list
; remember to rewrite w/ let*
(define (m-operation)
  (define (loop l-gem n)
    (if (= 0 n)
        l-gem
        (let ((next-gem (go-mine)))
          (let ((updated-gem-list (cons next-gem l-gem)))
            (loop updated-gem-list (- n 1))))))
  (loop '() 20))

(define gems (m-operation))
          
     
(define (miner.v2 m gem n)
  (define (mining n)
    ; 'm' is a mine of some type
    (cond
      ((= 0 n) (displayln "\nMiner wore out!"))
      (else
       (if (char=? (m) gem)
           (begin
             (printf "\nGem of type '~a' is found!\n" gem)
             gem) 
           (begin
             ;(sleep 0.1)
             (displayln "\nMining...")
             (mining (- n 1)))))))
  (mining n))

(for ((i 100))
  (miner.v2 mine gem 26))







  