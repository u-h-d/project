#lang racket

(define (mk-mmach mach-num)
       (list
        (list 'drill 50)
        (list 'power 100)
        (list 'chassis 200)
        (list 'storage 20)
        (list 'm-num mach-num)))

(define (make-miners n)
  (cond
    ((= 0 n) '())
    (else
     (cons (mk-mmach n)
           (make-miners (- n 1))))))

; attr selectors ->
(define (drill l)
  (car l))
(define (power-cell l)
  (car (cdr l)))
(define (chassis l)
  (car (cdr (cdr l))))
(define (storage l)
  (car (cdr (cdr (cdr l)))))
(define (number l)
  (car l (cdr (cdr (cdr (cdr l))))))

(define (next-machine m)
  (cond
    ((null? m) '())
    (else
     (cons (next-attr m)
           (next-machine (cdr m))))))

(define (next-attr m)
  (if (pair? (car m))
      (let ((tag (car (car m)))
            (value (car (cdr (car m)))))
        (printf "\nTag is ~a\n" tag) ; could do this where 
        (list tag
              (cond
                ((symbol=? 'drill tag) (- value 1))
                ((symbol=? 'power tag) (- value 1))
                ((symbol=? 'chassis tag) value)
                ((symbol=? 'storage tag) (- value 1))
                ((symbol=? 'm-num tag) value)
                (else
                 (displayln "Invalid machine attribute selector")))))
        ('NOT-PAIR)))
  
(define test-m
  (mk-mmach 1))

(define m-group
  (make-miners 20))

(define (mining-loop)
  ; cycle through the list of miners
  (define (cycle miners)
       (cond
         ((null? miners) '())
         (else
          (cons (next-machine (car miners))
                (cycle (cdr miners))))))
  ; clock ticks...
  (define (pass-time n new-miners)
    (cond
      ((= 1 n)
       (begin
         (displayln "Ending simulation")
         (cycle new-miners)))
      (else
       (let ((next-miners (cycle new-miners)))
         (pass-time (- n 1) next-miners))))) 
  (pass-time 5 m-group))

(define (cycle miners)
       (cond
         ((null? miners) '())
         (else
          (cons (next-machine (car miners))
                (cycle (cdr miners))))))


     
       