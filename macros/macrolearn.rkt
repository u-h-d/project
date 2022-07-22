#lang racket

(require (for-syntax racket/match))


(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr] ; 'datum->syntax' uses first arg ('stx' here)
                               [else ,false-expr]))])) ; as CONTEXT for creating the syntax w/ second arg

(define-syntax (our-if-using-syntax-case stx)
    (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]))


