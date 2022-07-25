#lang racket

(require "location.rkt")

; ______________________________________________________________________________________________________
; -----------------------------------------MINER--------------------------------------------------------


; a miner is a list of: control and list of: attr
; a control is a function that dictates miner behaviour
; enumerated attr are: drill, storage, and loc


; miner selectors
(define (control m)
  (car m))
(define (drill m)
  (car (cdr m)))
(define (storage m)
  (car (cdr (cdr m))))
(define (loc m)
  (car (cdr (cdr (cdr m)))))


          
; a sample miner
(define ms
  (list #f (list 100 0 (0 0))))


; ***MINER PROCESSING FUNCTIONS***


; miner -> direction
; a direction is a number
; determines the direction a miner will move next
(define (dir? m env)
  
  ; extract needed values from miner...
  (define m-loc (loc m))
  (define m-x (x m-loc))
  (define m-y (y m-loc))

  ; extract needed info from our environment...
  (define env-attr-sel

  ; loop to find suitable direction 
  (define (dir-rec n flag)

    (let* ((n-loc (update-loc m-loc n))
           (in-bounds (in-bounds? n-loc))
           (empty (member? n-loc known)))
      
      ;(printf "in-bounds is ~a\nn-loc is ~a\nn is ~a\n" in-bounds n-loc n)
      (cond
        ((and in-bounds
              (not empty))
         n-loc)
        ((and in-bounds flag)
         n-loc)
        (else
         (cond
           ((and (= n 3)
                 flag)
            '())
           ((= n 3)
            ;(displayln "Hi")
            (dir-rec 0 #t))
           (else
            ;(displayln "Hello")
            (dir-rec (+ n 1) flag)))))))
             
  (dir-rec 0 #f))