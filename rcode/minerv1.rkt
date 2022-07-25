#lang racket

; a miner is a controller (function of two args), a list of attr, and a loc 
; movement and mining function utilize state of environment ->

; constructor for a miners attributes...
(define (mk-attr mach-num)
  (list
   (list
    (list 'm-num mach-num)
    (list 'power 100)
    (list 'chassis 200)
    (list 'storage 20))
   '()
   '()))

; selectors for miners attributes
(define (attr miner)
  (car (cdr miner)))

(define (loc miner)
  (car (cdr (cdr miner))))

(define (contr miner)
  (car miner))

(define (move-f miner)
  (car (cdr (cdr (attr miner)))))

(define (mine-f miner)
  (car (cdr (attr miner))))
  
; miner controller...
(define (m-contr.v1 miner)
  (displayln "Initiating miner!"))

; constructor for a miner
(define (mk-miner contr attr loc)
  (cons contr
        (cons attr
              (cons loc '()))))

; test miner
(define test-m
  (mk-miner m-contr.v1 (mk-attr 1) (list 1 2)))


; start miner
(define (initiate-m miner)
  (let ((contr (contr miner)))
    (contr miner)))


; update a miner 
(define (update-miner miner)
  0)
       