#lang racket


(define (z)
  (thread (lambda ()
            (for ((i 10))
              (displayln "z")))))

(define (t)
  (thread loop))

(define (loop)
  (displayln "Hello")
  (loop))

(define (main)
  (t)
  (z))

(define (driver-loop)
  (define main-cust (make-custodian))
  (parameterize ((current-custodian main-cust))
    (thread (lambda ()
              (let loop ()
                (displayln "Here we go...")
                (sleep 0.5)
                (loop)))))
  (thread (lambda ()
            (sleep 5)
            (custodian-shutdown-all main-cust))))

#|
(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))
|#

(call-with-output-file "mine"
  #:exists 'truncate
    (lambda (out)
      (write "hello" out)))

#|
(define worker (thread (lambda ()
                         (let loop ()
                           (displayln "Working...")
                           (sleep 0.2)
                           (loop)))))
|#


(define (tt)
  (define z
    (thread (lambda ()
              (let loop ()
                (displayln "Working...")
                (sleep 0.2)
                (loop)))))
  (lambda ()
    (kill-thread z)))

(define q (tt))
(sleep 3)
(q)

;(sleep 2.5)
;(kill-thread worker)

(define worker-thread (thread
                       (lambda ()
                         (let loop ()
                           (match (thread-receive)
                             [(? number? num)
                              (printf "Processing ~a~n" num)
                              (loop)]
                             ['done
                              (printf "Done~n")])))))
(for ([i 20])
  (thread-send worker-thread i))
(thread-send worker-thread 'done)
(thread-wait worker-thread)

(define (test)
  (define (one x)
    (two x))
  (define (two x)
    (* 2 x))
  (one 1))

