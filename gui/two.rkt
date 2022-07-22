#lang racket

(require racket/gui)
(require racket/draw)
(require (for-syntax syntax/parse))
(require racket/math)
 

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
 
; Show the frame by calling its show method
(send frame show #t)

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(define z
  (new my-canvas% (parent frame)))


(define context (send z get-dc))


(define (loop x)
  
  (let i-loop ((n 0))
    (if (< n 20)
        (begin
          (displayln "here")
          (send context draw-rectangle (+ 1 (* n 10)) (+ 1 (* n 10)) (* n 10) (* n 10))
          (sleep 0.5)
          (i-loop (+ 1 n)))
        (send context clear)))
  (if (> x 10)
      'END
      (loop (+ 1 x))))

(define (draw-stuff)
  ; functions ->
  (define (str->lst str)
    (string->list str))
  (define (lst->nums lst)
    (for/list ((char lst))
      (char->integer char)))
  (define (ints->sum lst)
    (apply + lst))

  ; expects a string
  (define (contr input)
    (send context draw-rectangle 10 10 200 200)
    (send context draw-text "CONTR" 11 11)
    (send context set-text-foreground "blue")
    (sleep 0.5)
    (define o1 (open-output-string))
    (write input o1)
    (define o2 (open-output-string))
    (write (ints->sum (lst->nums (str->lst input))) o2)
    (send context draw-text (get-output-string o1) 11 30)
    (sleep 0.5)
    (send context draw-text "->" 120 30)
    (sleep 0.5)
    (send context draw-text (get-output-string o2) 140 30))

  (contr "Hello there"))

(define (ani)
  (for ((i 50))
    (sleep 0.1)
    (send context draw-rectangle (+ i 10) 100 50 50)))

(define target
  (make-bitmap 100 100))

(define tar-dc (new bitmap-dc% (bitmap target)))

(define blue-brush (new brush% [color "blue"]))
(define red-brush (new brush% (color "red")))
(send tar-dc set-brush blue-brush)

(send tar-dc draw-rectangle 5 5 50 50)

(require racket/math)
 
(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
;(define blue-brush1 (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))
 
(define (draw-face dc)
  (send dc set-smoothing 'aligned)
 
  (send dc set-pen no-pen)
  (send dc set-brush blue-brush)
  (send dc draw-ellipse 25 25 100 100)
 
  (send dc set-brush yellow-brush)
  (send dc draw-rectangle 50 50 10 10)
  (send dc draw-rectangle 90 50 10 10)
 
  (send dc set-brush no-brush)
  (send dc set-pen red-pen)
  (send dc draw-arc 37 37 75 75 (* 5/4 pi) (* 7/4 pi)))
 
(define target1 (make-bitmap 150 150))
(define dc (new bitmap-dc% [bitmap target1]))
 
(draw-face dc)

(send dc erase)
(send dc scale 0.5 0.5)
(draw-face dc)
target1
(send dc rotate (/ pi 2))
(send dc translate 0 150)
(draw-face dc)
target1
(send dc translate 0 -150)
(send dc rotate (/ pi 2))
(send dc translate 150 150)
(draw-face dc)
target1
(send dc translate -150 -150)
(send dc rotate (/ pi 2))
(send dc translate 150 0)
(draw-face dc)
target1

(define (draw-sq sz)
  (send context set-brush blue-brush)
  (send context draw-rectangle 50 50 sz sz)
  (send context set-brush red-brush)
  (send context draw-rectangle 60 60 sz sz))

(define (dm-maker)
  (let* ((frame (new frame% (label "Draw HERE"))) 
         (canvas (new canvas% (parent frame)))
         (dc (send canvas get-dc))
         (rect-base 200)
         (text-base 30)
         (continuation #f))
    (lambda (name input scale)
      (send dc draw-rectangle  5 5 (* scale rect-base) (* scale rect-base))
      (send dc draw-text 6 6 (* scale text-base) (* scale text-base)))))

(define (draw-entity name lo-comp lo-attr dc)
  (define scale 1)
  (define start 5)
  (define offset 5)
  (define num-comp (length lo-comp))
  (define big-side 500)
  (define third (/ big-side 3))
  (define splits (* 5 (+ 1 num-comp)))
  (define small-side (/ (- big-side splits) num-comp))
  (send dc draw-rectangle start start big-side big-side)
  (send dc draw-text name (+ 1 start) (+ 1 start))
  (for ((i num-comp))
    (send dc draw-rectangle (+ offset (* i (+ small-side offset)))(+ third offset) small-side small-side)))
  

        
(define (gh lst)
  (lambda ()
    (let ((x (car lst)))
      (set! lst (cdr lst))
      x)))


 