#lang racket

; mutually defined data structure seems maybe impossible w/out mutation ->
; to stay functional, may have to take a diff route ->
; function that generates cells, and adds the result to a data structure ->
; how to modify -> have to create whole new data structure each time?

(define BOUND 100)

(define (get-comp)
  (gensym))

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

(define (mk-locproc x y)
  (lambda (req)
    (cond
      ((symbol=? req 'x)
       x)
      ((symbol=? req 'y)
       y)
      (else
       'INCORRECT-REQUEST))))


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

(define (mk-row y)
  (define (inner x result)
    (cond
      ((= 0 x)
       result)
      (else
       (let* ((loc (mk-loc (- x 1) y))
              (new-cell (mk-cell loc))
              (new-result (cons new-cell result)))
         (inner (- x 1) new-result)))))
  (inner BOUND '()))

(define (mk-plane d)
  (define (inner y result)
    (cond
      ((= 0 y)
       result)
      (else
       (let* ((new-row (mk-row y))
              (new-result
               (cons new-row result)))
         (inner (- y 1) new-result)))))
  (inner BOUND '()))


; mk-earth
(define (mk-earth)
  ; core helper
  (define (inner d result)
    
    ; mk-plane
    (define (mk-plane)
      
      ; core helper
      (define (inner y result)
        
        ; mk-row
        (define (mk-row)
          
          ; core helper
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

    ; mk-earth core body
    (cond
      ((= 0 d)
       result)
      (else
       (let* ((new-plane (mk-plane))
              (new-result
               (cons new-plane result)))
         (inner (- d 1) new-result)))))
  
  ; mk-earth body
  (inner BOUND '()))


  
; extract a cell from a given earth object ->
(define (return-cell loc e)
  ; extract coords 
  (define z (get-z loc))
  (define x (get-x loc))
  (define y (get-y loc))

  ; core helper
  (define (find-cell)
    
    ; to find cell, first extract proper row from the appropriate plane ->
    (define (get-row)
      
      ; to find row, first find our proper plane
      (define (get-plane)
        (define (core counter sub-part)
          (cond
            ((= 1 counter) (car sub-part))
            (else (core (- counter 1) (cdr sub-part)))))
        (core z e))
      (define needed-plane (get-plane))
      
      ; get-row core, loop on proper plane to find row
      (define (core counter sub-part)
        (cond
          ((= 1 counter) (car sub-part))
          (else (core (- counter 1) (cdr sub-part)))))
      ; get-row body
      (core y needed-plane))
    (define needed-row (get-row))

    ; find-cell core
    (define (core counter sub-part)
        (cond
          ((= 0 counter) (car sub-part))
          (else (core (- counter 1) (cdr sub-part))))) 
    (core x needed-row))
  (find-cell))

(define q (mk-earth))
(define (tester)
  (return-cell '(96 27 52) q))

    
    
  
 
    
         
         
                             
    

     
  
  