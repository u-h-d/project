#lang racket

(require racket/gui)

(define (fill dc color)
  (let-values (((w h) (send dc get-size)))
    (send dc set-brush color 'solid)
    (send dc draw-rectangle 0 0 w h)))

#;(define (fill-all)
  (fill dc0 "green"))
  ;(fill dc1 "blue")
  ;(fill dc2 "red"))


  
  

; # machines/2, if not whole (ie odd #) round up
; vertical panes of appropriate height w/ new panes top & bottom

; layer1
(define fr (new frame%
                (label "test")
                (alignment '(left center))
                #;(border 30)
                #;(spacing 20)
                #;(alignment '(left center))
                #;(stretchable-width #f)
                #;(stretchable-height #f)))




(define (mk-panes&dc n parent)
  (for/list ((i n))
    (send
     (new canvas% (parent
                   (new pane%
                        (parent fr)
                        (border 5)
                        (min-width 100)
                        (min-height 50)
                        (stretchable-width #f)
                        (stretchable-height #f)))
          (paint-callback
                  (lambda (canvas dc)
                    (let-values (((width height) (send dc get-size)))
                      (send dc set-brush "blue" 'solid)
                      (send dc draw-rectangle 0 0 (- width 0) (- height 0))))))
     get-dc)))

(define (paint-blue dcs)
  (for ((dc dcs))
    (send dc set-brush "blue" 'solid)))

(define (draw-rects lo-dc)
  (for ((dc lo-dc))
    (let-values (((width height) (send dc get-size)))
      (send dc set-brush "blue" 'solid)
      (send dc draw-rectangle 5 5 (- width 5) (- height 5)))))

(define dcs
  (mk-panes&dc 15 fr))
;(paint-blue dcs)


#|
(define p0 (new horizontal-pane% (parent fr)
                (alignment '(left top))
                #;(border 20)
                #;(spacing 30)
                (min-width 200)
                (min-height 200)
                (stretchable-width #f)
                (stretchable-height #f)))
(define c0 (new canvas%
                (parent p0)
                #;(paint-callback fill-all)))
(define dc0 (send c0 get-dc))
|#

#|
; layer2
(define lp (new horizontal-pane%
                (parent fr)
                #;(alignment '(left center))
                #;(border 0)
                (min-width 200)
                (min-height 200)
                (stretchable-width #f)
                (stretchable-height #f)))
(define c1 (new canvas%
                (parent lp)))
(define dc1 (send c1 get-dc))
|#

#|
(send p0 set-alignment 'right 'bottom)

(define lp2 (new horizontal-pane%
                (parent p0)
                #;(alignment '(left center))
                #;(border 0)
                (min-width 200)
                (min-height 200)
                (stretchable-width #f)
                (stretchable-height #f)))
(define c2 (new canvas%
                (parent lp2)))
(define dc2 (send c2 get-dc))
|#


#|
; layer3
(define lp2 (new horizontal-pane%
                (parent lp)
                (alignment '(left center))
                (border 15)))
(define c2 (new canvas%
                (parent lp2)))
(define dc2 (send c2 get-dc))
|#
                

(send fr show #t)