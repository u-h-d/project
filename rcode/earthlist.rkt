#lang racket

; earth
; the earth is a bunch of locations w/ attributes ->
; location is coord (x y z) and list-of attr
; attr are temp and humidity and composition
; temp and humidity are numbers, composition is a character

; size of 2-d grid
(define BOUNDS 10)
; max depth
(define MAX-DEPTH 10)

(define (mk-location coord)
  (cons coord
        (cons (get-attr coord) '())))

(define (get-attr coord)
  (cons (get-comp coord)
        (cons (get-temp coord)
              (cons (get-hum coord) '()))))

(define (get-comp coord)
  #\z)
(define (get-temp coord)
  0)
(define (get-hum coord)
  0)

(define (mk-row depth y)
  (letrec ((row
            (lambda (n)
              (let ((coord (list n y depth)))
                (cond
                  ((= BOUNDS n) '())
                  (else (cons (mk-location coord)
                              (row (+ n 1)))))))))
    (row 0)))

(define (mk-plane depth)
  (define (inner n)
    (cond
      ((= BOUNDS n) '())
      (else (cons (mk-row depth n)
                  (inner (+ n 1))))))
  (inner 0))

(define (mk-earth)
  (define (inner n)
    (cond
      ((= BOUNDS n) '())
      (else (cons (mk-plane n)
                  (inner (+ n 1))))))
  (inner 0))

; selectors for locations and components ->
(define (

; movement functions
; arg is symbol
(define (move-row direction earth)
  (define (move-up)
    (let ((cur-row (
  (cond
    ((symbol=? 'up)
     (move-up))
    ((symbol=? 'dn)
     (move-down))
    (else
     'INVALID INPUT)))
            
                 
                        
    
  
