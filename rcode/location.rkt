#lang racket

(provide x
         y
         update-loc)

; a location is a list of two numbers, an x and y
; we assume a miner will operate in a context that supports this representation of location

; loc selectors
(define (x loc)
  (car loc))
(define (y loc)
  (car (cdr loc)))




; return (build) a new loc based on old location moving a given direction
(define (update-loc loc dir)
  (cond
    ((= dir 0)
     (list (+ (x loc) 1)
           (y loc)))
    ((= dir 1)
     (list (x loc)
           (+ (y loc) 1)))
    ((= dir 2)
     (list (- (x loc) 1)
           (y loc)))
    (else
     (list (x loc)
           (- (y loc) 1)))))