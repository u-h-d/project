#lang racket

(require racket/gui)

; frame
(define fr (new frame% (label "hello")
                (alignment '(center top))
                (width 1024)
                (height 946)
                (min-width 1024)
                (min-height 946)
                (stretchable-width #f)
                (stretchable-height #f)
                (border 20)))

; seems like whatever TYPE of child (horiz, vert) you place in frame affectts
; layout
#;
(define dc
  (send
   (new canvas% (parent fr)
        (min-width 300)
        (min-height 250)
        (stretchable-width #f)
        (stretchable-height #f))
   get-dc))

; pane0

(define p0 (new horizontal-pane%
                (parent fr)
                #;(min-width 250)
                #;(min-height 250)
                #;(stretchable-width #f)
                #;(stretchable-height #f)))

(define c0 (new canvas%
                (parent p0)))

(define dc0
  (send c0 get-dc))

; pane1

(define p1 (new horizontal-pane%
                (parent p0)
                (alignment '(right top))
                #;(border 20)
                #;(min-width 50)
                #;(min-height 50)
                #;(stretchable-width #f)
                #;(stretchable-height #f)))
(define c1 (new canvas%
                (parent p1)))
(define dc1
  (send c1 get-dc))


; pane2

(define p2 (new vertical-pane%
                (parent p1)
                (border 10)
                (min-width 50)
                (min-height 50)
                (stretchable-width #f)
                #;(stretchable-height #f)))
(define c2 (new canvas%
                (parent p2)))
(define dc2
  (send c2 get-dc))


(define (fill dc color)
  (let-values (((w h) (send dc get-size)))
    (send dc set-brush color 'solid)
    (send dc draw-rectangle
          0
          0
          w h)))

(define (mk n parent layout)
  (for/list ((i n))
    (send
     (new canvas%
          (parent
           (new vertical-pane%
                (parent p1)
                (border 0)
                (min-width 50)
                (min-height 50)
                (stretchable-width #f)
                #;(stretchable-height #f))))
     get-dc)))

(define (fa)
  (fill dc0 "green")
  (fill dc1 "red")
  (fill dc2 "blue"))

(define fr2 (new frame% (label "hello")
                (alignment '(center top))
                (width 1024)
                (height 946)
                (min-width 1024)
                (min-height 946)
                (stretchable-width #f)
                (stretchable-height #f)
                (border 20)))


(define cz0
  (new canvas% (parent fr2)))
(define dz0
   (send cz0 get-dc))
(define cz1
  (new canvas% (parent fr2)))
(define dz1
  (send cz1 get-dc))
(define pz0
  (new horizontal-pane% (parent fr2)))
(define cp0
  (new canvas% (parent pz0)))
(define cdc0
  (send cp0 get-dc))
   

  
  
(send fr2 show #t)
;(send fr show #t)