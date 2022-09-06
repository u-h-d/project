#lang racket

(require racket/gui)

; set up CANVAS
(define fr (new frame% (label "hello")))

(define c0 (new canvas% (parent fr)
                #;(paint-callback
                 (lambda (canvas dc)
                   (dcd)))
                #;(paint-callback
                 (lambda (canvas dc)
                   (draw
                    (init-w/dc
                     (m-space-r)
                     dc)
                    dc)))
                #;(paint-callback
                 (lambda (canvas dc)
                   ; fixed
                   (draw
                    (find-in-super
                     t-space
                     '(0 0 1000 1000))
                    dc)
                   ; not-fixed
                   (draw
                    (init-w/dc
                     2-space
                     dc)
                    dc)))))


                    
(define dc
  (send c0 get-dc))

(send fr show #t)

; !! maybe instead of a data structure have a lambda that contains the data structure
; that proc can take a fixed-super and derive
; have a closure where can either derive when asked OR be fixed based on flag
; can also add in translations

; data definitions for SPACES & FIXED SPACES ->

; a space is: (list-of: (list-of: r-w r-h) alignment (list-of: children))
; where r-w and r-h are NUM
; alignment is (list of SYMBOL SYMBOL)
; children are SPACES

; space needs to be a closure, and have the ability to draw TO it
; need a way to ROTATE a space

; example ->
#;
`((.3 .3) (left center) (,c1 ,c2))

; space selectors ->
(define (scale space)
  (car space))
(define (r-w space)
  (first (scale space)))
(define (r-h space)
  (second (scale space)))
(define (alignment space)
  (second space))
(define (sub-spaces space)
  (third space))

; FIXED SPACE

; (list-of: x0 y0 w h (list-of sub-spaces
; where x0 y0 w h is a NUM, and sub-spaces are FIXED SPACE

; example ->
#;
`(456.567 234.4 924 589 (((3 5 10 12) ())
                         ((4 7 49 10) ())))

; selectors ->
(define (fixed-data fixed-space)
  (first fixed-space))
(define (s-x0 fixed-space)
  (first (fixed-data fixed-space)))
(define (s-y0 fixed-space)
  (second (fixed-data fixed-space)))
(define (s-width fixed-space)
  (third (fixed-data fixed-space)))
(define (s-height fixed-space)
  (fourth (fixed-data fixed-space)))
(define (fixed-sub-spaces fixed-space)
  (second fixed-space))

; find-pt
(define (find-pt alignment super-pt super-dimension space-dimension)

  (if (symbol? alignment)
      (case alignment
        ((left) super-pt)
        ((top) super-pt)
        ((center)
         (+ super-pt (- (/ super-dimension 2) (/ space-dimension 2))))
        ((right)
         (- (+ super-pt super-dimension) space-dimension))
        ((bottom)
         (- (+ super-pt super-dimension) space-dimension)))
      (+ super-pt (* alignment super-dimension))))

; find-in-super
; assumes super-space has been fixed first and recurs through data structure fixing the spaces
; in eaches superior
(define (find-in-super space fixed-super-space-data)

  ;(printf "~a\n~a\n" space fixed-super-space-data)
  
  (let* ((super-x0 (first fixed-super-space-data))
         (super-y0 (second fixed-super-space-data))
         (super-width (third fixed-super-space-data))
         (super-height (fourth fixed-super-space-data))
         (space-width (* super-width (r-w space)))
         (space-height (* super-height (r-h space)))
         (alignment (alignment space))
         (space-x0 (find-pt (first alignment) super-x0 super-width space-width))
         (space-y0 (find-pt (second alignment) super-y0 super-height space-height)))
    
    (let ((new-fixed-space `(,space-x0 ,space-y0 ,space-width ,space-height)))

      `(,new-fixed-space
        
        ,(for/list ((sub (sub-spaces space)))
             (find-in-super sub new-fixed-space))))))

(define (draw fixed-space dc)
  (if (null? fixed-space)
      'DONE
      (let ((nxt (car fixed-space)))
        (if (null? nxt)
            (void)
            (cond
              ((number? (car nxt))
               (begin
                 (send dc draw-rectangle (first nxt) (second nxt) (third nxt) (fourth nxt))
                 (draw (cdr fixed-space) dc)))
              (else
               (begin
                 (draw (car fixed-space) dc)
                 (draw (cdr fixed-space) dc))))))))
      
          
          

; initiialize w/ frame
(define (init-w/dc space dc)
  
  (let-values (((w h) (send dc get-size)))
    (find-in-super space
                   `(0 0 ,w ,h))))

(define (init space fixed-space-data)
  (find-in-super space fixed-space-data))

; TESTS

(define n
  `((.25 .25) (center center) ()))
(define z
  `((.1 .5) (right bottom) ,(list n)))
(define q
  `((.7 .8) (center center) ,(list z)))

(define t-space
  `((.3 .3) (left center) ,(list q)))
(define 2-space
  `((.2 .2) (center bottom) ()))

(define (m-space #;(name lst-comps lst-attr))
  `((1 1) (center center)
           (((.4 .9) (left bottom)
                     (#;((.25 .25) (.1 top)
                                 (#;,(m-space)))))
            ((.4 .9) (right bottom) ())
            )))

(define (m-space-d #;(name lst-comps lst-attr))
  `((1 1) (center center)
           (((.4 .9) (left top)
                     (#;((.25 .25) (.1 top)
                                 (#;,(m-space)))))
            ((.4 .9) (right top) ())
            )))

(define (m-space-r #;(name lst-comps lst-attr))
  `((1 1) (center center)
           (((.9 .4) (left top)
                     (#;((.25 .25) (.1 top)
                                 (#;,(m-space)))))
            ((.9 .4) (left bottom) ())
            )))

(define (m-space-l #;(name lst-comps lst-attr))
  `((1 1) (center center)
           (((.9 .4) (right top)
                     (#;((.25 .25) (.1 top)
                                 (#;,(m-space)))))
            ((.9 .4) (right bottom) ())
            )))
  
; maybe should have way to only fix down so far...
(define (proc msg)
  0)

(define (rect)
  (let-values (((w h) (send dc get-size)))
    (let ((x0 0)
          (y0 0))
      `(,x0 ,y0 ,w ,h))))

(define r
  '(5 5 50 50))

(define (dr r)
  (send dc draw-rectangle (car r) (second r) (third r) (fourth r)))

; will need a translation attached to a space
; OR, just define the flipped spaces
(define (rot dir)
  (case dir
    ((right)
     (lambda (fixed-space)
       (let ((x0 (first fixed-space))
             (y0 (second fixed-space))
             (w (third fixed-space))
             (h (fourth fixed-space)))
         `(,(- w (+  h y0))
           ,y0
           ,h
           ,w))))))

(define (close lo-spaces)
  (for/list ((space lo-spaces))
    (let ((sp space))
      (lambda (msg)
        sp))))

(define lo-spaces
  (list t-space 2-space))


; example
; takes tree of entity id's and applies this layout to each
(define (init-e-spaces ent)

  (define (e-spaces name comps flag)
    ;(printf "name: ~a\ncomps: ~a\n" name comps)
    
    (let* ((s1 (truncate (/ (length comps) 2)))
           (s2 (- (length comps) s1))
           (s1-comps (take comps s1))
           (s2-comps (list-tail comps s1))
           (s1-scale (if (= 0 s1)
                         1
                         (/ 1 s1)))
           (s2-scale (if (= 0 s2)
                         1
                         (/ 1 s2))))

      ;(printf "s1: ~a\ns2: ~a\ns1-scale: ~a\ns2-scale: ~a\n" s1 s2 s1-scale s2-scale)
      ; building a list of the total layout ->
      `(((.4 .9) (left bottom)
                 (,@(if (not (null? comps))
                        (for/list ((sub s1-comps)
                                   (i (in-range 0 (length s1-comps))))
                          `((1 ,s1-scale) (center ,(+ 0 (* s1-scale i)))
                                          ,(e-spaces (car sub) (cadr sub) 'right)))
                        '())))
        ((.4 .9) (right bottom)
                 (,@(if (not (null? comps))
                        (for/list ((sub s2-comps)
                                   (i (in-range 0 (length s2-comps))))
                          `((1 ,s2-scale) (center ,(+ 0 (* s2-scale i)))
                                          ,(e-spaces (car sub) (cadr sub) 'right)))
                        '()))))))

  `((1 1) (center center)
          ,(e-spaces (car ent) (cadr ent) 'up)))

(define e1
  '(m0 ((m1 ((m5 ()) (m10 ())))
        (m2 ((m3 ((m4 ())
                  (m11 ())))
             (m12 ())))
        (m6 ((m7 ()) (m8 ())))
        (m9 ()))))

(define e
  '((0 0 100 100)
    (((10 10 30 30) ())
     ((20 20 40 40) (((30 30 60 60) ()))))))

(define o
  (init-e-spaces e1))
(define b
  (find-in-super
   o
   '(0 0 1000 500)))

(define (wrap-all fixed-spaces)

  (define (wrap fixed-space)
    (let ((space fixed-space))
      (lambda (cmd)
        space)))
  
  `(,(wrap (car fixed-spaces))
    ,(for/list ((sub (second fixed-spaces)))
       ;(displayln sub)
        (wrap-all sub))))

(define (draw-proc procs dc)
  (if (null? procs)
      (void)
      (let ((nxt (car procs)))
        ;(displayln nxt)
        (if (procedure? nxt)
            (let ((lst (nxt 'l)))
              (begin
                (send dc draw-rectangle (first lst) (second lst) (third lst) (fourth lst))
                (draw-proc (cdr procs) dc)))
            (begin
              (draw-proc nxt dc)
              (draw-proc (cdr procs) dc))))))

(draw-proc (wrap-all b) dc)

; works to...
(define (fedp procs dc)
  
  (define (dr lst)
    (if (null? lst)
        (void)
        (if (pair? lst)
            (fedp lst dc)
            (let ((lst (lst 'l)))
              (send dc draw-rectangle (first lst) (second lst) (third lst) (fourth lst))))))
  
  (for-each dr procs))


(define f (make-font #:size 2))


; is this an attempt at ...?
(define (space coords dc)
  
  (let ((x0 (first coords))
        (y0 (second coords))
        (w (third coords))
        (h (fourth coords))
        (dc dc)
        (draw-proc #f)
        (lst-translations #f)
        (path-counter #f)
        (path #f))

       
    (lambda (cmd)
      
  ;    (displayln "hi")
   ;   (displayln cmd)
    ;  (displayln (car cmd))
      (if path-counter
          ; if active path, translate
          ; might need separate function that keeps/manages paths/translations
          ; if we have an active path-counter, calc our current x and y ->
          (let ((new-x0 (+ (* (car path-counter) (car path)) x0))
                (new-y0 (+ (* (car path-counter) (second path)) y0)))
            
            (begin0
 ;               (printf "inside path branch: \n" )
                (case (car cmd)

                  ((set-proc) (set! draw-proc (cadr cmd)))
                  ((new-path) (set! path (cadr cmd)))
                  ((dump)
                   `(,new-x0 ,new-y0 ,w ,h ,dc ,draw-proc ,lst-translations ,path-counter ,path))
                  ; apparently this is for if we want to pass a custom drawing procedure to this space?
                  ((dp)
                   ((cadr cmd) new-x0 new-y0 w h dc))
                  ((draw)
                   (draw-proc new-x0 new-y0 w h dc))
                  ((path)
                   (set! path-counter (build-list (car (second cmd)) values))
                   (set! path (cadr cmd)))
                  (else
                   'ERROR))
              ; move path-counter to next ->
                (set! path-counter (cdr path-counter))))
          (begin
;          (printf "inside no-path branch: \n" )
          (case (car cmd)
            ((set-proc) (set! draw-proc (cadr cmd)))
            ((new-path) (set! path (cadr cmd)))
            ((dump)
             `(,x0 ,y0 ,w ,h ,dc ,draw-proc ,lst-translations ,path-counter ,path))
            ((dp)
             ((cadr cmd) x0 y0 w h dc))
            ((draw)
             (draw-proc x0 y0 w h dc))
            ((path)
             (set! path-counter (build-list (car (second cmd)) values))
             (set! path (second (cadr cmd))))
            (else
             'ERROR)))))))
  

(define (drect x y w h dc)
  (sleep 0.1)
  (send dc draw-rectangle x y w h))
(define (dt x y w h dc)
  (sleep 0.05)
  (send dc draw-text "hello" (+ x 5) (+ y 5)))

(define c
    (space '(100 100 200 200) dc))

(define (gen-lst)
  (let ((lst '(10 1 1)))
    (lambda ()
      (begin
        (displayln (car lst))
        (set! lst (cdr lst))))))

(c '(path (500 (2 0))))
(c `(set-proc ,drect))

(define (dcd)
  (define (draw dc)
    (c '(draw)))
  (for ((i 500))
    (sleep 0.0000005)
    (send c0 refresh-now draw)))


          
     

