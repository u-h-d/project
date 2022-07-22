#lang racket


(define (f l)
  (if (null? l)
      '()
      ; core
      (let ((a (car l))
            (b (cdr l)))
        (lambda ()
          (a b)))))

(define (add x)
  (+ 1 x))

(define l
  (list add
        (list 4 5 6 7 8)))

(define l2
  '(1 2 3 4 5 6 7 8))
            


; take a list and make a thunk
(define (mk l)
  (let ((f (car l))
        (first (car (cdr l)))
        (rest (cdr l)))
    (lambda ()
      (f first))))

(define (flow f)
  (define (core n)
    (cons (lambda ()
            (f n))
          (lambda ()
            (core (+ n 1)))))
  (core 1))

(define stream
  (flow add))

(define (extract stream n)
  (if (null? stream)
      '()
      (let ((val ((car stream)))
            (rest ((cdr stream))))
        (printf "val is ~a\nrest is ~a\n" val rest)
        (cond
          ((= 0 n) '())
          (else
           (cons val
                 (extract rest (- n 1))))))))

(define (f-m s)
  (printf "stream is ~a\n" s)
  (let ((val ((car s)))
        (rest ((cdr s))))
    (printf "val is ~a\nrest is ~a\n" val rest)
    (if (= (modulo 117 val) 0)
        val
        (f-m rest))))

(define (test s)
  (printf "stream is ~a\n" s)
  ((car stream)))
             
  
  
(define result
  (extract stream 10))
  
    
              


  
  