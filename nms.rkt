#lang racket

(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

(define-syntax (test stx)
  (syntax-parse stx
    ((_test id)
     (with-syntax ((n-id (format-id #'id "~a" (datum->syntax #'id 'v))))
       #'(begin
           (define n-id 10)
           (define-syntax (id stx)
             (syntax-parse stx
               ((_id identifier)
                #'identifier))))))))

(define-syntax (t2 stx)
  (syntax-parse stx
    ((_t2 ((ids vals) ...))
     (with-syntax ((list-id (format-id #'_t2 "~a" (datum->syntax #'_t2 'lst)))
                   (m-id (format-id #'_t2 "~a" (datum->syntax #' _t2 'get))))
       #'(begin
           (displayln 'ids) ...
           (displayln 'vals) ...
           (define list-id
             `(,'(ids vals) ...))
           (define-syntax (m-id stx) ; first tried to use bare syntax, didn't work, could use pattern var or created id...
             (syntax-parse stx
               ((_get ident)
                #'ident))))))))
  
(t2 ((a 10) (b 4)))

(define z 1)
(define y 2)

(define-syntax (t3 stx)
  (syntax-parse stx
    ((_t3 pairs)
     (with-syntax ((((id val) ...) #'pairs))
       #'(begin
           (displayln 'id) ...
           (displayln 'val) ...)))))


(define ht
  (make-hash
   (list '(controller . #f)
         `(input-hooks . ,(lambda (cmd)
                            'hello)))))

(define (g)
  (define b 4)
  (lambda (val)
    (printf "Before: ~a" b)
    (set! b val)
    (printf "After: ~a" b)))

(define h
  (g))

(define p
  '(sym . 4))


(define-syntax (mk-n stx)
  (syntax-parse stx
    ((_mk-n id)
     #'(define id 4))))

    
(define-syntax (t4 stx)
  (syntax-parse stx
    ((_t4 id)
     #'(mk-n id))))

(define-values
  (s x)
  (values
   (lambda ()
     'hello)
   (lambda ()
     'here)))
