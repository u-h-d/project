#lang racket


; if file is growing too large, add worker
; if gets too small, shutdown worker

; how about dynamically changing the WORK RATE of workers to balance...?

(define (rs size)
  ; make a string of random chars of size 'size' ->
  (define (l-o-char)
    (for/list ((i size))
      (integer->char (random 65 91))))
  (let ((l (l-o-char)))
    (list->string l)))

(define (input-gen)
  (displayln "Mining...")
  (call-with-output-file "mine"
    #:exists 'append
    (lambda (out)
      (write (rs 1000) out)))
  (sleep 2)
  (input-gen))

(define (tt)
  (thread
   (input-gen))
  (thread
   (for ((i 10))
     (displayln "hi"))))

(define (launch-worker)
  (thread (lambda ()
            (displayln "Worker thread...")
            (let loop ()
              (displayln "Working....")
              (define payload
                (call-with-input-file "mine"
                  (lambda (in)
                    (read-string 100 in))))  ; how to actually EXTRACT...?
              (call-with-output-file "stockpile"
                #:exists 'append
                (lambda (out)
                  (write payload out)))
              (sleep 3)
              (loop))))
  (lambda () 
    (custodian-shutdown-all (current-custodian))))


; ok, before 'input-gen' was locking out 'worker'... not sure why ->
; NOW, they both make progress...
(define (d-l)
  (thread input-gen)
  (define end (launch-worker))
  (sleep 10)
  (end))

(define (worker pat mine stockpile)
  (0))
  ; search for first word in mine that satisfies regexp pattern and place in stockpile
  ; need to EXTRACT the word so characters are actually removed from mine... how?

; could do a deal where worker takes random word from dictionary and then searches
; can record all sorts of stats about the work 


#|
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))
|#
       
                
   



#|
(define (accept-and-handle listener)
  (define cust (make-custodian)) ; local custodian 'cust'
  (custodian-limit-memory cust (* 50 1024 1024)) ; limit memory w/in this cust to protect attacks
  (parameterize ([current-custodian cust]) ; 'cust' will become 'current-custodian' for this body/block
    (define-values (in out) (tcp-accept listener)) ; 'tcp-accept' creates TWO streams (for input/output)
    (thread (lambda () ; make new thread w/ thunk that calls 'handle' w/ streams
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread:
  (thread (lambda () ; value of 'accept' is this thread that sleeps 10 then shuts everything down...?
            (sleep 10)
            (custodian-shutdown-all cust))))
|#