#lang racket

; seems call/cc is syntatic sugar for what you would otherwise write in CPS style
; https://stackoverflow.com/questions/39493092/what-is-the-continuation-of-call-cc-in-a-let-block

; **making PARAMTERS, which can be DYNAMICALLY BOUND, are preferrable to using set!

; In (+ 1 (+ (+ 3 4) 2)), when (+ 3 4) has been evaluated, the
; continuation is (+ 1 (+ [] 2)), where [] represents what has already
; been done

(define BOUND 25)

; either CPS or accumulator...
(define (get-comp accum)
  (let ((flag (random 100)))
    (if (< flag 6)
        (list #\z (round (/ accum 3)))
        (list #f 0))))

; mapping location and size of deposits
(define (map-deposits)
  (define num (* BOUND BOUND))
  (define (m-d-rec counter accum dec)
    (if (= num counter)
        '()
        (if (> dec 0)
            (cons (list #\z #f) (m-d-rec (+ 1 counter) 0 (- dec 1)))
            (let* ((comp (get-comp accum))
                   (ore (car comp))
                   (sz (car (cdr comp))))
              (if ore
                  (cons comp (m-d-rec (+ 1 counter) 0 sz))
                  (cons comp (m-d-rec (+ 1 counter) (+ 1 accum) 0)))))))
  (m-d-rec 0 0 0))

(define q (map-deposits))

(define (pred i)
  (equal? (car i) #\z))

(filter pred q)

(define (next-f n col)
  (let ((sum (col n)))
    (cons sum
          (lambda (x)
            (next-f x
                    (lambda (y)
                      (+ n y)))))))

(define (a-friend n)
  (+ 1 n))

(define z (next-f 5 a-friend))

(define (sum)
  (define (sum-rec l ctr)
    (cond
      ((= 0 ctr)
       '())
      (else
       (let* ((n-pair (next-f ctr a-friend))
              (n-val (car n-pair))
              (n-f (cdr n-pair)))
         (sum-rec (cons n-val
                        (n-f ctr))
                  (+ 1 ctr))))))
  (sum-rec '() 10))


; try a data object that is a pair of: func and list ->
; func generates next list item then generates its next version ->
; need to know when we start a deposit and when/how to end it ->
; do we track all nodes, or just ore deposits? ->
; our 'map' is just a list of ore locations and a function to return the comp
; at a given location
(define (create-comp cur loc)
  ; func to get the composition of a given location
  ; uses known results and location
  (define (get-comp loc)
    (let ((comp? (known? result)))
      (cond
        ((comp?) comp?)
        (else
  
  (define f (car cur))
  (define result (car (cdr cur)))
  
  
  

    
  


         
                         
    
  
       
  
  
