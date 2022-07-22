#lang racket

(require (for-syntax syntax/parse))


; code to make a message
; 'message' is structure type within given fields assoc with the name of the structure type 

; macro to (return a function?) make a struct type based on given name and field list
(define l
  '(name length field))
(define n
  'test)

(define-syntax-rule (mk-st name lst)
  `(struct ,name (,@lst)))
 

(define-syntax (mk-struct stx)
    (syntax-parse stx
      ((_ name lst)
       (displayln stx)
       (displayln (syntax->datum #'name))
       (displayln (syntax->datum #'lst))
       (displayln (syntax->datum #'(struct name lst)))
       #'(struct name lst))))

(define-syntax (mylet stx)
    (syntax-parse stx
      [(_ ([var-id rhs-expr] ...) body ...+)
       #'((lambda (var-id ...) body ...) rhs-expr ...)]))

(define-syntax (hello stx)
    (syntax-case stx ()
      [(_ name place)
       (with-syntax ([print-name #'(printf "~a\n" 'name)]
                     [print-place #'(printf "~a\n" 'place)])
         #'(begin
             (define (name times)
               (printf "Hello\n")
               (for ([i (in-range 0 times)])
                    print-name))
             (define (place times)
               (printf "From\n")
               (for ([i (in-range 0 times)])
                    print-place))))]))