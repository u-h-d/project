#lang racket


; ______________________________________________________________________________________________________
; ---------------------------------------ENVIRONMENT----------------------------------------------------

; an environment is a list of entities, list of known locations, and a function that returns an uknown nodes
; attributes ->

; env selectors
(define (entities env)
  (car env))
(define (known env)
  (car (cdr env)))
(define (attr env)
  (car (cdr (cdr env))))
  

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