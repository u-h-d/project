#lang racket

; just saved this to new name, wanted to preserve where we couldn't scale bc it had to draw NEW
; grid every loop, and hung, wanted to keep as lesson

(require 2htdp/image
         2htdp/universe)


(define BOUND 99)



  





     
    
    
#|
(define t
  (thread
   (lambda ()
     (world))))

(sleep 10)
(kill-thread t)
|#


(define t-m
  '(((0 0) (0 1))
    ((1 0) (1 1))))


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

(define l1
  (mk-coord))

(define (trim l new-l)
  ; trim the row...
  (define (t-row r new-r)
    (cond
      ((null? r)
       new-r)
      (else
       (define loc (car r))
       (define flag (= 0 (random 2)))
       (if flag
           (t-row (cdr r) (cons loc new-r))
           (t-row (cdr r) new-r)))))
  ; cycle through rows
  (cond
    ((null? l)
     new-l)
    (else
     (define accum-result (cons (t-row (car l) '()) new-l))
     (trim (cdr l) accum-result))))
     
  
        
(define (draw-grid n l)
  ; draw colored square nxn
  ; place black sub squares wherever mining has occurred
  (define w (empty-scene 1000 1000 "gray"))
  (define sq (square 100 "solid" "black"))
  ; outer loop
  (define (d-grid-rec l scene-so-far)
    ; inner loop
    (define (d-rec sub-l scene-so-far)
      (cond
        ((null? sub-l)
         scene-so-far)
        (else
         (define next (car sub-l))
         (define x-pos (+ 50 (* 100 (x next))))
         (define y-pos (+ 50 (* 100 (y next))))
         (cond
           ((null? sub-l)
            scene-so-far)
           (else
            (d-rec (cdr sub-l)
                   (place-image sq x-pos y-pos scene-so-far)))))))
    (cond
      ((null? l)
       scene-so-far)
      (else
       (define next-row (car l))
       (d-grid-rec (cdr l)
                   (d-rec next-row scene-so-far)))))
  ; initiate outer loop
  (d-grid-rec l w))

(define i-grid
  (empty-scene 1000 1000 "gray"))

(define sq (square 10 "solid" "black"))

(define (update-grid scene-so-far l-updates)
  ;(printf "next pair thing is ~a\n" (car l-updates))
  (if (null? l-updates)
      scene-so-far
      (let* ((next (car l-updates))
             (pos (car next))
             (x-pos (+ 5 (* 10 (x pos))))
             (y-pos (+ 5 (* 10 (y pos))))
             (image (car (cdr next)))
             (new-scene (place-image image x-pos y-pos scene-so-far)))
        (update-grid new-scene (cdr l-updates)))))

;(define w1 (reverse (world)))
;(define w2 (trim w1 '()))

;(draw-grid 0 w2)

(define (t-row r new-r)
    (cond
      ((null? r)
       new-r)
      (else
       (define loc (car r))
       (define flag (= 0 (random 2)))
       (if flag
           (t-row (cdr r) (cons loc new-r))
           (t-row (cdr r) new-r)))))

(define row
  '((0 0) (0 1) (0 2)))

; ______________________________________________________________________________________________________
; --------------------------------------------WORLD-----------------------------------------------------

; env(0) (blank env)
(define (mk-env)
  (list (list '(100 0 (0 0))) '(((0 0)))))

(define env (mk-env))

(define env-scene (update-grid (empty-scene 1000 1000 "gray") '()))
  
; env -> env
(define (update-env)
  ;(printf "Known is ~a\nIt's size is ~a\n" (known env) (size? (known env)))
  (let*
      ((current-miner (car (miners env)))
       (current-known (known env))
       (next-loc (dir? current-miner current-known)))
    ;(printf "next-loc is ~a\n" next-loc)
    (cond
      ((= (* (+ 1 BOUND) (+ 1 BOUND)) (size? current-known))
       (printf "Mining complete: Number of known locations is ~a\n" (size? current-known)))
      (else
       (let*
           ((next-env (mining-result current-miner next-loc current-known))
            (next-miner (miners next-env))
            (next-known (known next-env))
            (new-scene (update-grid env-scene (list (list next-loc sq)))))
         ;(next-image (draw-grid 0 next-known)))
         ; (displayln next-miner)
         ; (displayln next-known)
         ; (displayln (length next-known))
         ;(sleep 1)
         (set! env next-env)
         (set! env-scene new-scene))))))
        

(define (get-scene n)
  (begin
    (update-env)
    env-scene))




  
 




         
     

    
    
    
    