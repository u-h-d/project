#lang racket

(define test
  (let ((inner 10))
    (lambda (x)
           (if x
               (set! inner x)
               (begin
                 (displayln inner)
                 (displayln 'NO_CHANGE))))))

; want to have a way to take a collection/list of values (say name/value pairs) and create a closure with them...

(define (mk-node attr)

  ; create lexical env (node attributes)
  (let ((name attr))
    
    ; node controller ->
    (lambda ([x #f])

      ; <subordinate node functions> ->

      ; is this a valid network message?
      (define (ntwk_msg? data)
        (and (pair? data)
             (symbol=? 'msg (car data))))

      ; new network msg
      (define (n-msg msg-info)
        (define address (car msg-info))
        (define payload (car (cdr msg-info)))
        (list 'msg address payload))
      
      ; handler for validated network messages
      (define (net-msg-handler msg)
        (define contents (cdr msg))
        (define address (first contents))
        (define payload (second contents))
        (printf "We have received a message: ~a\nStarting a relaltionship (and thread) with sender...\n" contents)
        ; would be a good place to use continuation to insert a response back where we first received the msg?
        ; if no response required, then continuation will end relationship
        (thread
         (lambda ()
           (thread-send address (list (current-thread) (+ 1 payload) 0))
           (let loop ()
             (let* ((n-msg (thread-receive))
                    (address (car n-msg))
                    (val (second n-msg))
                    (loops (third n-msg)))
               (printf "New msg is: ~a\nVal is ~a\n" n-msg val)
               (if (< val 5)
                   (begin
                     (printf "Q still incrementing, at ~a\n" val)
                     (sleep 0.5)
                     (thread-send address (list (current-thread) (+ 1 val) (+ 1 loops)))
                     (loop))
                   'ALL_DONE))))))
      
      ; general dispatch
      (cond
        ((and x
              (ntwk_msg? x))
         (net-msg-handler x))
        (else 'ERROR)))))


(define-values (q z)
  (values
   (mk-node '(name of-stuff))
   (mk-node '(here is-two))))

#;
(define (initiate msg)
  (thread
   (lambda ()
     (q '(msg (current-thread) 1))
     (let loop ()
       (let ((val (thread-receive)))
         (if (< val 5)
             (begin
               (printf "Still incrementing, at ~a" val)
               (sleep 0.5)
               (thread-send address (current-thread) (+ 1 payload))
               (loop))
             'ALL_DONE))))))



(define tester 
  (thread
   (lambda ()
     (q (list 'msg (current-thread) 1))
     (let loop ()
       (let* ((n-msg (thread-receive))
              (address (car n-msg))
              (val (second n-msg))
              (loops (third n-msg)))
         (printf "New Tester msg is: ~a\nVal is ~a\n" n-msg val)
         (if (< val 5)
             (begin
               (printf "Tester still incrementing, at ~a\n" val)
               (sleep 0.5)
               (thread-send address (list (current-thread) (+ 1 val) (+ 1 loops)))
               (loop))
             (printf "Done at ~a\n" val)))))))

(sleep 5)
(kill-thread tester)
#;
(define thread1
  (thread
   (lambda ()
     ;(displayln "here1")
     (thread-send thread2 1)
     (let loop ()
       (let ((val (thread-receive)))
         (if (< val 20)
             (begin
               (printf "We are at ~a\n" val)
               (thread-send thread2 (+ 1 val))
               (sleep 0.5)
               (loop))
             (displayln 'DONE1)))))))
#;
(define thread2
  (thread
   (lambda ()
     ;(displayln "here2")
     (let loop ()
       (let ((val (thread-receive)))
         (if (< val 20)
             (begin
               (printf "We are at ~a\n" val)
               (thread-send thread1 (+ 1 val))
               (sleep 0.5)
               (loop))
             (displayln 'DONE2)))))))




               
                 


