#lang racket

; there are rand generators that will produce the same results if started w/ same seed...

(define BOUND 25)

; comp list of characters that goes to depth of BOUND
; drill can only go straight down
; make deposits by stacking chance to have one, rand for size, then stacking chance to terminate
; how to get deposits radially? -> would have to do that in mk-plane...
(define (get-comp)
  )

(define (get-temp)
  0)

(define (get-hum)
  0)

; constructor for a location (a pair of numbers)
; should redo this PROCEDURALLY 
(define (mk-loc x y z)
  (cons x
        (cons y
              (cons z '()))))


; selectors for location object
(define (get-x loc)
  (car loc))
(define (get-y loc)
  (car (cdr loc)))
(define (get-z loc)
  (car (cdr (cdr loc))))

(define (mk-cell loc)
  (cons loc
        (cons
         (list
          (get-comp)
          (get-temp)
          (get-hum))
         '())))

; mk-plane
(define (mk-plane d)
  
  ; core helper
  (define (inner y result)
    
    ; mk-row
    (define (mk-row)
      
      ; core helper -> could we use CPS in our mk-cell to allow stacking chance for deposit...?
      (define (inner x result)
        (cond
          ((= 0 x)
           result)
          (else
           (let* ((loc (mk-loc (- x 1) y d))
                  (new-cell (mk-cell loc))
                  (new-result (cons new-cell result)))
             (inner (- x 1) new-result)))))
      ; mk-row body
      (inner BOUND '()))
    
    ; mk-plane core body
    (cond
      ((= 0 y)
       result)
      (else
       (let* ((new-row (mk-row))
              (new-result
               (cons new-row result)))
         (inner (- y 1) new-result)))))
  
  ; mk-plane body
  (inner BOUND '()))