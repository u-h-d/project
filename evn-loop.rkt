#lang racket

(require 2htdp/image
         2htdp/universe)


(define BOUND 24)

; ______________________________________________________________________________________________________
; --------------------------------------------WORLD-----------------------------------------------------


; an world is an env, parameters and a function that is env(0) -> env(1) -> env(n)
; an env is a list of miner and a list of mined (empty) spaces
(define (world)

  ; world paramters
  

  ; env(0) (blank env)
  (define (mk-env)
    (list (list ms) '(((0 0)))))
  
  ; env -> env
  (define (env-rec env n)
    ;(printf "Known is ~a\nIt's size is ~a\n" (known env) (size? (known env)))
    (let*
        ((current-miner (car (miners env)))
         (current-known (known env))
         (next-loc (dir? current-miner current-known)))
      (draw-grid 0 current-known)
      (cond
        ((= (* (+ 1 BOUND) (+ 1 BOUND)) (size? current-known))
         (printf "Mining complete: Number of known locations is ~a\n" (size? current-known)))
        (else
         (let*
             ((next-env (mining-result current-miner next-loc current-known))
              (next-miner (miners next-env))
              (next-known (known next-env)))
              ;(next-image (draw-grid 0 next-known)))
           ; (displayln next-miner)
           ; (displayln next-known)
           ; (displayln (length next-known))
           ;(sleep 1)
           (env-rec next-env (+ n 1)))))))
  (env-rec (mk-env) 0))

; ______________________________________________________________________________________________________
; ---------------------------------------ENVIRONMENT----------------------------------------------------

; an environment is a list of miners(entities), list of known locations, and a function that returns a nodes
; composition ->

; env selectors
(define (miners env)
  (car env))
(define (known env)
  (car (cdr env)))


; ***PROCESSING OF ENV***

; when a node is mined, we determine if ore was found ->
; get composition of a node (composition is 1 for ore, 0 for rock)
(define (get-comp loc)
  (let ((flag (random 10)))
    (if (< flag 2)
        1
        0)))

; the result of a successful mining action on the env at a node location is:
; a new env, which is an updated miner and a new known location
; comps are either rock, ore, or empty -> all we need to track is empty, bc rock/ore determined
; dynamically
(define (mining-result m n-loc known)

  ;(printf "Miner is now at location ~a\n" n-loc)

  ; find out what the comp is
  (define comp (get-comp n-loc))
  
  ; update miners attr (ie build a new one)
  (define new-miner
    (list (- (drill m) 1)
          (+ (storage m) comp)
          n-loc))

  ; (printf "This is n-loc ~a\n" n-loc)
  
  ; we return a new env, which is a list of updated miners and list of mined locations
  (list (list new-miner)
        (if (member? n-loc known)
            known
            (insert-loc n-loc known))))

; PREDICATE
; is a proposed location valid by the BOUNDS of our map?
; loc -> boolean
(define (in-bounds? loc)
  
  ; extract our x and y from loc
  (let ((x (x loc))
        (y (y loc)))
    ; return bool
    (and (and (>= x 0)
              (>= y 0))
         (and (<= x BOUND)
              (<= y BOUND)))))
  

; ______________________________________________________________________________________________________
; -----------------------------------------MINER--------------------------------------------------------


; a miner is a list of attr
; enumerated attr are: drill, storage, and loc
; miner attr selectors
(define (drill m)
  (car m))
(define (storage m)
  (car (cdr m)))
(define (loc m)
  (car (cdr (cdr m))))

; a location is a list of two numbers, an x and y
; loc selectors
(define (x loc)
  (car loc))
(define (y loc)
  (car (cdr loc)))
          
; a sample miner
(define ms
  '(100 0 (0 0)))


; ***MINER PROCESSING FUNCTIONS***

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

; miner -> direction
; a direction is a number
; determines the direction a miner will move next
(define (dir? m known)
  
  ; extract needed values from miner...
  (define m-loc (loc m))
  (define m-x (x m-loc))
  (define m-y (y m-loc))

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


; probably need an 'insert' function to insert a known loc in proper order...
(define (insert-loc loc map)
  ; (printf "Location coming in is ~a\n" loc)
  ; extract our x and y
  (define x-loc (x loc))
  (define y-loc (y loc))
  ; helper procedure to insert a loc in a row once we find the right one...
  (define (insert-in-row row y-loc)
    ;(printf "Row coming in is ~a\n" row)
    (if (null? row)
        (list loc)
        (let* ((cur-loc (car row))
               (cur-y (y cur-loc)))
          (cond
            ((= y-loc cur-y)
             row)
            ((> y-loc cur-y)
             (cons cur-loc
                   (insert-in-row (cdr row) y-loc)))
            (else
             (cons loc row))))))

  ; if the map is empty, just return the location, bc it IS our map now
  (if (null? map)
      (list (list loc))
      ; otherwise, there is at least one row ->
      ; extract it and find out what num row it is
      (let* ((row (car map))
             (row-num (x (car (car map)))))
      (cond
        ; if its the right row, cons the result of inserting the loc into this row
        ; onto the rest of the map
        ((= row-num x-loc)
         (cons (insert-in-row row y-loc)
               (cdr map)))
        ; else, cons this row unchanged, onto the result of recurring with the rest of map
        (else
         (cons row
               (insert-loc loc (cdr map))))))))
      
(define (size? map)
  (define (s-rec map n)
    (cond
      ((null? map)
       n)
      (else
       (let ((row-length (length (car map))))
         (s-rec (cdr map) (+ n row-length))))))
  (s-rec map 0))

(define (member? loc l)
  (define x-loc (x loc))
  (define y-loc (y loc))
  
  ; helper to search a given row
  (define (search-row row)
    ;(displayln "Entering search-row...")
    (cond
      ((null? row)
       #f)
      (else
       (let ((cur-y (y (car row))))
         (cond
           ((= y-loc cur-y)
            #t)
           (else
            (search-row (cdr row))))))))
  
  ; core rec to find the proper row
  (define (m-rec l)
    (cond
      ((null? l)
       #f)
      ((pair? (car l))
       (let ((row (car l))
             (row-num (x (car (car l)))))
         ;(printf "row is ~a\nrow-num is ~a\n" row row-num)
         (cond
           ((= x-loc row-num)
            (search-row row))
           ((> x-loc row-num)
            (m-rec (cdr l)))
           (else
            #f))))
      (else
       #f)))
  
  ; initiate core...
  (m-rec l))
     
    
    
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

; takes a map of coord, and randomly trims...
(define (trim l new-l)
  
  ; trim a row helper
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
  (define sq (square 10 "solid" "black"))
  ; outer loop
  (define (d-grid-rec l scene-so-far)
    ; inner loop
    (define (d-rec sub-l scene-so-far)
      (cond
        ((null? sub-l)
         scene-so-far)
        (else
         (define next (car sub-l))
         (define x-pos (+ 5 (* 10 (x next))))
         (define y-pos (+ 5 (* 10 (y next))))
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

(define w1 (world))
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
 




         
     

    
    
    
    