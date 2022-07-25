#lang racket

(require "location.rkt")


; makes a list of coord
(define (mk-coord)
  (define SL 100)
  (define (mk-rec l x y)
    (if (= x SL)
        l
        (let ((flag (random 3)))
          (cond
            ((and (= flag 1)
                  (< y SL))
             (let ((n-l (cons (list x y) l)))
               (mk-rec n-l x (+ y 1))))
            ((< y SL)
             (mk-rec l x (+ y 1)))
            (else
             (mk-rec l (+ x 1) 0))))))
  (mk-rec '() 0 0))

; could do this where map across initiated list of empty loc's, cons y, then x onto empty locs...

(define (mk-grid n)
  (define (mk-row y)
    (define (rec x row)
      (cond
        ((< x 0) row)
        (else
         (rec (- x 1) (cons (list x y) row)))))
    (rec (- n 1) '()))
  (define (grid-rec y grid)
    (cond
      ((< y 0) grid)
      (else
       (grid-rec (- y 1) (cons (mk-row y) grid)))))
  (grid-rec (- n 1) '()))

; list -> list
; utility to take a complete grid and randomly trim out locations
; 'n' determines chance a given loc is trimmed
(define (trim-grid grid n)
  (define (trim? x)
    (if (= 1 (random n))
        #t
        #f))
  (for/list ((l grid))
    (filter trim? l)))

; list -> image
(define (draw-grid grid)
  0)
    
      
 