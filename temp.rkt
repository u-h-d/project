#lang racket

(require racket/stxparam (for-syntax racket/syntax syntax/parse racket/stxparam))

(define t

  (let ((val 4))

    (lambda (cmd)

      (case (car cmd)

        ((set) (set! val (second cmd)))
        ((get) val)))))


(define ctr
  
  (lambda (x)
    (let ((val (t '(get))))
      (+ x val))))

(define z
  (begin
    (let ((y 1))
      (lambda (x)
        (+ x y)))))


(define-syntax-rule (mk-return name)
  (begin
    (define (name)
             'hello)
    name))

(define b 'a)

(define lst
  '(a b c))

(define (invoke lst)
  (mk-return (car lst)))

(define-syntax (mk-name stx)
  (syntax-parse stx
    ((_mk-name sym pvt-id)

     #'(define-syntax sym
         (syntax-rules (set get)
           ((sym set val) (pvt-id '(set val)))
           ((sym get) (pvt-id '(get))))))))

(define (mk-closure sym)

  (let ((name sym))
    (lambda (cmd)
      (case (car cmd)
        ((get) (printf "My symbol is ~a\n" name))
        ((set)
         (begin
           (displayln (second cmd))
           (set! name (second cmd))))))))

(define-syntax (definterf stx)
  (syntax-parse stx
    ((_definterf name)
     #:with secret-id (format-id #'name "~a/~a" (syntax-e #'name) 'e)
     #'(begin
         (define secret-id (mk-closure 'name))
         (mk-name name secret-id)))))

(define-syntax-parameter t-param
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "only used inside deftclr")))


(define-syntax (tester stx)
  (syntax-parse stx
    ((_tester body)
     
     ;#:with n-id (format-id #'body "~a" 'n-id)
     
     #'(let ((n-id 4))
         (syntax-parameterize ((t-param (make-rename-transformer #'n-id)))
           body)))))


(define-syntax (mat stx)
  (syntax-parse stx
    ((_mat expr ... result)

     #'(begin
         expr ...
         result))))
