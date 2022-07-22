#lang racket

(require "utilities.rkt")

; BITCOIN SCRIPT ->
; a script is a set of valid script values (constants or operators)
; constants are pushed onto stack, and operators do various things (push, pop, calculate, etc)
; a transaction is VALID if top value after execution is #t, non-zero or empty... huh
; INVALID if #f, or early termination


; a script is a listof: script values
                ; script values: constant (string or number)
                              ; | operator

; test script
(define ts-unlock
  '(1 2 op_add))
(define ts-lock
  '(3 op_equal))

(define l/ul-s
  (append ts-unlock ts-lock))

; a stack is a list

(define (op_add stack)
  (let ((num1 (first stack))
        (num2 (second stack)))
    (cons (+ num1 num2) (cddr stack))))

(define (op_equal stack)
  (let ((num1 (first stack))
        (num2 (second stack)))
    (= num1 num2)))

(define (op_push val stack)
  (cons val stack))

(define (op_checksig stack)
  (let* ((lock-pub-key (car stack))
         (sig (second stack))
         (test-pub-key (priv->pub sig)))
    (equal? test-pub-key lock-pub-key)))

(define op-list
  `((op_add ,op_add) (op_push ,op_push) (op_equal ,op_equal) (op_checksig ,op_checksig)))

(define (concat-scr lock unlock)
  `(,@unlock ,@lock))


(define (eval-script script)
  (let loop ((n 0)
             (scr script)
             (stack '()))
    (printf "Remaining script is ~a\nStack is ~a\n" scr stack)
    (cond
      ((null? scr) stack)
      (else
       (let ((scr-val (car scr)))
         (cond
           ((assoc scr-val op-list)
            (loop (+ 1 n) (cdr scr) ((second (assoc scr-val op-list)) stack)))
           ((or (number? scr-val)
                (string? scr-val)
                (bytes? scr-val))
            (loop (+ 1 n) (cdr scr) (op_push scr-val stack)))
           (else
            (error "Invalid script value in script"))))))))

; TESTS
(define test-priv-key
  (random 1000000))
(define test-pub-key
  (priv->pub test-priv-key))

(define test-lock-scr
  `(,test-pub-key op_checksig))
(define test-unlock-scr
  `(,test-priv-key))
(define test-concat-scr
  (concat-scr test-lock-scr test-unlock-scr))
(define invalid-scr
  (concat-scr test-lock-scr '(4)))


             
    


        