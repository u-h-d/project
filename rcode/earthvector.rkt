#lang racket

; ***OK, need to consider custom list structure using lists/pairs
; might kinda be like a graph... node w/ neighbors

; NODES
; node is a value and 6 neighbors
; could use gensym...

(define (cubrec n)
  (define (inner x)
    (if (= 0 n)
        '()
        (list
         (cons 1 (cubrec (- n 1)))
         (cons 2 (cubrec (- n 1)))
         (cons 3 (cubrec (- n 1)))
         (cons 4 (cubrec (- n 1))))))
  (inner 1))

; a cubes neighbors are 1-6

  
(define BOUND 5)
(define MAX-DEPTH 5)

(define (mk-earth)
  ; initial vector to build off of...
  (define top-row (make-vector BOUND))
  
  ; take a vector and put a vector of equal size in all of v's slots
  ; IF we haven't hit our dimension limit
  (define (build v d)
    (cond
      ((= d 0) v)
      (else
       (let ((sz (vector-length v))) 
         ; inner loop
         (define (inner n)
           (cond
             ((= (- sz 1) n)
              (begin
                (vector-set! v n (build (make-vector sz) (- d 1)))
                v))
             (else
              (begin
                (vector-set! v n (build (make-vector sz) (- d 1))) ; side effect
                (inner (+ n 1))))))
         (inner 0))))) 
  (begin
    (build top-row 3)
    top-row)) 
           


(define z (make-vector 20))

