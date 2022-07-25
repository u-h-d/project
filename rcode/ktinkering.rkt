#lang racket

(require 2htdp/image
         2htdp/universe)



(empty-scene 100 100 "red")

(define (a-number digit)
    (overlay
     (text (number->string digit) 12 "black")
     (circle 10 "solid" "white")))

(define inner-dial
    (overlay
     (text "555-1234" 9 "black")
     (circle 30 "solid" "white")))


(define (place-and-turn digit dial)
    (rotate 30
            (overlay/align "center" "top"
                           (a-number digit)
                           dial)))

(define (place-all-numbers dial)
    (foldl place-and-turn
           dial
           '(0 9 8 7 6 5 4 3 2 1)))

 (define (rotary-dial f)
    (scale
     f
     (overlay
      inner-dial
      (rotate
       -90
       (place-all-numbers (circle 60 "solid" "black"))))))

(define (create-UFO-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))
 
(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))
 
;(animate create-UFO-scene)


(define saved-k #f)


(define (save-it!)
  (call-with-composable-continuation
   (lambda (k) ; k is the captured continuation
     (set! saved-k k)
     0)))

; what about a delimited continuation...?
(define (k-test)
  (define (k-test-rec n)
    (printf "Iteration ~a\n" n)
    (let/cc here
      (set! saved-k here))
    (lambda ()
      (k-test-rec (+ n 1))))
  (k-test-rec 0))

(define (c-test)
  (define (c-rec n)
    (printf "Iteration ~a\n" n)
    (lambda ()
      (c-rec (+ n 1))))
  (c-rec 0))
  
(define (extract f)
  (define (e-rec f n)
    (if (= n 20)
        (begin
          (displayln "Done")
          f)
        (let ((n-f (f)))
          (e-rec n-f (+ n 1)))))
  (e-rec (f) 0))

(define new-f (extract k-test))

(define new-c (extract c-test))