#lang racket

(require (for-syntax syntax/parse racket/syntax))

(define (id-filter ht)
  (let* ((ht-keys (hash-keys ht))
         (keys (filter (lambda (key)
                         (not (or (eq? 'input-hooks key)
                                  (eq? 'output-hooks key)
                                  (eq? 'controller key)
                                  (eq? 'controller-datum key)
                                  (eq? 'e-name key))))
                       (hash-keys ht))))
    keys))

(define a 10)
(define b 15)
(define c 12)

(module above racket
  (provide get-ids get-vals get-syn)
  
  (define get-ids
    (let ((ids '(a b c)))
      (lambda ((cmd #f))
        (if cmd
            (case (car cmd)
              ((set) (set! ids (second cmd))))
            ids))))

  (define get-syn
    (let ((id 'a))
      (lambda ((val #f))
        (if val
            (set! id val)
            (begin
              (datum->syntax #f id)
              id)))))

  (define get-vals
    (let ((vals '(1 2 3)))
      (lambda ((cmd #f))
        (if cmd
            (case (car cmd)
              ((set) (set! vals (second cmd))))
            vals)))))
  

(require 'above (for-syntax 'above))

(define-syntax (with-env stx)
  (syntax-parse stx
    ((_mk-run ((ids val) ...) body)

     (with-syntax (((n-id ...) (map (lambda (id)
                                      (displayln (syntax-e id))
                                      (format-id #'_mk-run "~a" (syntax-e id)))
                                    (syntax->list #'(ids ...))))
                   #;(a-id (format-id #'body "~a" 'a))
                   #;(b-id (format-id #'body "~a" 'b)))
       #;(displayln #'(n-id ...))
       
       #'(let ((n-id val) ...)
           body)))))


(define-syntax (td stx)
  (syntax-parse stx
    ((_td)
     
     (define ids (get-ids))
     (define vals (get-vals))

     (with-syntax (((n-ids ...) ids)
                   ((n-vals ...) vals))

       #'(begin
           (displayln n-ids) ...
           (displayln n-vals) ...)))))

(define-syntax (td2 stx)
  (syntax-parse stx
    ((_td2)
     #`#,(format-id #'_td2 "~a" (get-syn)))))

(define-syntax change
  (let ((id 'a))
    (lambda (stx)
      (syntax-parse stx
        ((_change e)
         #:with n-id (format-id #'e "~a" id)
         
         #'(let ((n-id 4))
             e))
        ((_change (val))
         (define n-val (syntax-e #'val))
         (displayln n-val)
         (set! id n-val)
         #'(displayln "set id"))))))

(define-syntax (t3 stx)
  (syntax-parse stx
    ((_t3 idr)
     (define new (- 20 (syntax-e #'idr)))
     #'new)))

(define e-build
  (let ((bindings '((a 4) (b 3))))
    (lambda (cmd)
      (cond
        ((= (length cmd) 1)
         (displayln 'here)
         (displayln
          (eval `(let ,bindings
                    ,(car cmd)))))
        (else
         (set! bindings (cadr cmd)))))))

(define zb
  '((a 4) (b 3)))

(define (ee e)
  (define expr
    `(let ,zb ,e))
  (eval
   expr))




#;(define (clos)

  (define components
    (make-hash
     '((a . 4)
       (b . 3)
       (controller . (+ a b)))))


  (define (call-w/env)
    (define keys
      (id-filter (hash-keys components)))
    (define bindings
      (map (lambda (key)
             `(,key ,(hash-ref components key)))
           keys))
    (define body (hash-ref components 'controller))
    (with-env bindings body))

  (call-w/env))

(define (tp (x #f))
  (if x
      x
      'NONE))
