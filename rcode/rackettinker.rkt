#lang racket

(define saved-k #f)
(define (save-it!)
    (call-with-composable-continuation
     (lambda (k) ; k is the captured continuation -> 
       (set! saved-k k)
       0)))

; (+ 1 (+ 1 (+ 1 (save-it!)))) ; when we call 'save-it!' the 'call-with' expression in its body will capture the program
                             ; context at THAT point (it goes up to the nearest 'prompt-tag'...?
                             ; so here it captures the entire nested expression ->
                             ; that CONTINUATION is handed to the local function which 'set!' s 'saved-k' and returns 0
; having the above expression in the definitions area causes it to return value twice...
; but using it the REPL does not

; 'saved-k' is the name for a value which is that very program context, or CONTINUATION
; and that continuation is ENCAPSULATED so it acts like a function that takes a value and plugs it in ->
; (lambda (v) (+ 1 (+ 1 (+ 1 v))))
(define sk2 #f)
(define (con2)
  (call/cc
   (lambda (k)
     (set! sk2 k)
     0)))

; a continuation captured by 'call-with-composable' will EXTEND the current continuation (they don't abort to prompt)
; whereas the continuation captured by 'call/cc' aborts to current prompt when applied

(define exceptionhandler
  (lambda (msg) (display "unhandled exception")))

(define (f n)
  (+ 5
     (if (zero? n) (exceptionhandler "division by zero")
         (/ 8 n))))

(define (g n)
  (+ (f n) (f n)))

(define (h)
  (let/cc k
    (begin
      (set! exceptionhandler (lambda (msg) (begin
                                             (displayln msg)
                                             (k))))
      (displayln (g 1))
      (displayln (g 0))
      (displayln (g 2)))))

(define (test x)
  (let/cc k
    continuations rackedt
    
    
