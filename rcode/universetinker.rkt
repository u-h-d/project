#lang racket

(require 2htdp/image
         2htdp/universe)


(define i-grid
  (empty-scene 1000 1000 "gray"))

(define sq
  (square 9 "solid" "black"))

; one way to loop and build a grid of 'n' size
(define (build-grid n)
  (for/fold ((scene i-grid)
             (x 0)
             (y 0))
            ((i (in-range n)))
    (values
     (place-image sq
                  (+ 5 (* 10 x))
                  (+ 5 (* 10 y))
                  scene)
     (if (= x 99)
         0
         (+ x 1))
     (if (= x 99)
         (+ y 1)
         y))))

; another way...
(define (b-grid n)
  (for*/fold ((scene i-grid)
              (count 0))
             ((x (in-range 0 100))
              (y (in-range 0 100)))
    #:break (= n count)
    (values
     (place-image sq
                  (+ 5 (* 10 x))
                  (+ 5 (* 10 y))
                  scene)
     (+ count 1))))

; custom recursive way...
(define (bg-rec n)
  (define (core count y scene)
    (define (loop-x x scene count)
      (cond
        ((= count n) scene)
        ((= 100 x)
         (core count (+ y 1) scene))
        (else
         (let ((n-scene (place-image sq
                                     (+ 5 (* 10 x))
                                     (+ 5 (* 10 y))
                                     scene)))
           (loop-x (+ x 1) n-scene (+ count 1))))))
    (cond
      ((= n count) scene)
      ((= 100 y) scene)
      (else
       (loop-x 0 scene count))))
  (core 0 0 i-grid))

(define (bgr)
  (define (core count y scene)
    (define (loop-x x scene count)
      (cond
        ((= count n) scene)
        ((= 100 x)
         (core count (+ y 1) scene))
        (else
         (let ((n-scene (place-image sq
                                     (+ 5 (* 10 x))
                                     (+ 5 (* 10 y))
                                     scene)))
           (loop-x (+ x 1) n-scene (+ count 1))))))
    (cond
      ((= n count) scene)
      ((= 100 y) scene)
      (else
       (loop-x 0 scene count))))
  (core 0 0 i-grid))
    

(define-values
  (im1 k)
  (b-grid 5000))
     
            


;(define im1
 ; (build-grid 9999))


    
     