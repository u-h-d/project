#lang racket

(require racket/gui)





(define (cmd-sq)

  (let ((fixed? #f)
        (pts '(0 0 .5 .5))
        )

    ; proc
    (lambda (msg)
      (let ((tag (car msg))
            #;(content (cadr msg)))
        (case tag
          ((set)
           (let* ((content (cadr msg))
                  (x0 (car content))
                  (y0 (cadr content)))
             (begin
               (set! fixed? #t)
               (let-values (((w h) (send dc get-size)))
                 (set! pts `(,x0 ,y0 ,(* (third pts) w) ,(* (fourth pts) h)))))))
           ((draw)
           (lambda ()
             (send dc draw-rectangle (first pts) (second pts)
                   (third pts) (fourth pts))))
          ((make)
           (let* ((content (cadr msg))
                  (x0 (car content))
                  (y0 (cadr content)))
             (set! pts `(,(car content) ,(cadr content) (third content) (fourth content)))))
          ((dump)
           `(,fixed? ,pts)))))))

; just makes a row of squares...
(define (mv-sq pts)

  (let ((x0 (car pts))
        (y0 (cadr pts))
        (w (caddr pts))
        (h (cadddr pts)))
    ; path is a move and a number of times to make it..
    (lambda (path)
      (let ((dx (car path))
            (dy (second path))
            (mvs (third path)))
        (for ((i mvs))
          (send dc draw-rectangle
                (* i (+ dx x0))
                (* i (+ dy y0))
                w
                h))))))
; can have closure contain a current translation (if any) which can terminate OR restart
(define (mv-sq2 pts)

  (let ((x0 (car pts))
        (y0 (cadr pts))
        (w (caddr pts))
        (h (cadddr pts))
        (mvs '(.01 .01))
        (i '(1 500000)))
    
    ; path is a move and a number of times to make it..
    (lambda (msg)
      (case msg
        ((draw)
         (begin
           (if (= (cadr i) 0)
               (set! i '(1 500))
               (begin
                 (send dc draw-rectangle
                       (+ (* (car i) (car mvs)) x0)
                       (+ (* (car i) (cadr mvs)) y0)
                       w
                       h)
                 (set! i `(,(+ 1 (car i)) ,(- (cadr i) 1)))))))))))
      

(define u
  (mv-sq2 '(5 5 50 50)))


(define fr (new frame% (label "hi")))
(define zcan (new canvas% (parent fr)
                  (paint-callback
                   (lambda (canvas dc)
                     (u 'draw)))))


(define dc
  (send zcan get-dc))

(define (dn dc)
  (u 'draw))

(define (ani)
  (let loop ()
    (sleep 0.00005)
    (send zcan refresh-now
          dn #:flush? #t)
    (loop)))

(send fr show #t)