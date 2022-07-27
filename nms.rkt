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
  
;(t2 ((a 10) (b 4)))

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

(define f-lst
  '(lambda (cmd)
     (+ i
        l)))

(define-syntax (wkshp stx)
  (syntax-parse stx
    ((_wkshp name body ((id val) ...))
     (with-syntax ((n-body (datum->syntax #'name (syntax->datum #'body))))
     #'(begin
         (define name
           (let ((id val) ...)
             n-body)))))))

(define (t-contr cmd lst-comp)
  (let ((p1 (first lst-comp))
        (p2 (second lst-comp)))
    (p2 (p1 cmd))))


(define-syntax (n-contr stx)
  (syntax-parse stx
    ((_n-contr b-expr)
     (with-syntax ((bsyn (datum->syntax #f #'b-expr)))
       #'(lambda (cmd)
           (eval bsyn))))))

#;
 (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id rhs:expr)))

; **************************************************************************************************************************************
;;; WORKSHOP PROBLEM ->


;;; VERSION 1 :

(begin-for-syntax
  
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (define-syntax-class distinct-bindings
      #:description "sequence of distinct binding pairs"
      (pattern (b:binding ...)
               #:fail-when (check-duplicate-identifier
                            (syntax->list #'(b.var ...)))
                           "duplicate variable name"
               #:with (var ...) #'(b.var ...)
               #:with (rhs ...) #'(b.rhs ...))))

#;
(define-syntax (mk-wkshp stx)
  (syntax-parse stx
    ((_mk-wkshp name distinct-bindings)
     
     (with-syntax ((wkshp-id (format-id #'name "~a-wkshp" (syntax-e #'name))))
       
       #'(define-syntax (wkshp-id stx)
           (syntax-parse stx
             ((_wkshp-id (proc args (... ...)))
              #'(let distinct-bindings
                    (proc args (... ...))))))))))


;;; VERSION 2:


#;
(define-syntax (mk-wkshp2 stx)
  (syntax-parse stx
    ((_mk-wkshp2 name ((id val) ...))
     (with-syntax ((wkshp-id (format-id #'name "~a-wkshp" (syntax-e #'name)))
                   ((n-ids ...)
                    (begin
                      (let ((result
                             (let ((ids (syntax->list #'(id ...))))
                               (for/list ((ID ids))
                                 (format-id #'name "~a" (syntax-e ID))))))
                        (displayln result)
                        result))))
                      
       
       #'(define-syntax (wkshp-id stx)
           (syntax-parse stx
             ((_wkshp-id t-id)
              (let ((ids2 (syntax->list #'(n-ids ...))))
                (with-syntax (((nn-ids (... ...))
                               (for/list ((ID ids2))
                                 (format-id #'t-id "~a" (syntax-e ID))))
                              (n-vals val ...))
                  
              #'(let ((nn-ids n-vals) (... ...)
                  t-id)))))))))))


(define-syntax (mk-wkshp3 stx)
  (syntax-parse stx
    ((_mk-wkshp3 name ((id val) ...))
     (with-syntax ((wkshp-id (format-id #'name "~a-wkshp" (syntax-e #'name))))
                                        
       #'(define-syntax (wkshp-id stx)
           (syntax-parse stx
             ((_wkshp-id t-id)
              
              (with-syntax (((n-ids (... ...))
                             (begin
                               (let ((result
                                      (let ((ids (syntax->list #'(id ...))))
                                        (for/list ((ID ids))
                                          (format-id #'name "~a" (syntax-e ID))))))
                                 ;(displayln result)
                                 result)))
                              ((n-vals (... ...)) #'(val ...)))
                                          
                #'(begin
                    (displayln n-ids) (... ...)
                    (displayln n-vals) (... ...)
                    (let ((n-ids n-vals) (... ...))
                      (displayln (list n-ids n-vals)) (... ...)
                      t-id))))))))))

(define-syntax (mk-wkshp4 stx)
  (syntax-parse stx
    ((_mk-wkshp4 name ((id val) ...))
     
     ; collect the symbols from given syntax
     (define id-datums
       (map syntax-e
            (syntax->list #'(id ...))))
     (define id-syntax
       (map (lambda (datum)
              (format-id #'name "~a" datum))
            id-datums))
     (displayln id-syntax)
     (with-syntax (((n-ids ...) id-syntax)
                   ((n-vals ...) #'(val ...))
                   (wkshp-id (format-id #'name "~a-wkshp" (syntax-e #'name))))

       
       
       #'(define-syntax (wkshp-id stx)
           (syntax-parse stx
             ((_wkshp-id expr)
              (define n-id-syntax
                (map (lambda (datum)
                       (format-id #'expr "~a" datum))
                     (syntax->list #'(n-ids ...))))
              (with-syntax (((nn-ids (... ...)) n-id-syntax)
                            ((nn-vals (... ...)) #'(n-vals ...)))
                
                #'(let ((nn-ids nn-vals) (... ...))
                    expr)))))))))
                         
 ;; !!THIS ROTTEN FUCKER WORKS...                                 

(mk-wkshp4 zt ((a 4) (b 3)))

; wait, trying to CALL a PROC w/ unbound identifier...
(define-syntax (t5 stx)
  (syntax-parse stx
    ((_t5 name id)
     (with-syntax ((n-id (datum->syntax #'id 'a)))

       #'(define-syntax (name stx)
           (syntax-parse stx
             ((_name new-id)
              (with-syntax ((nnew-id (format-id #'new-id "~a" (syntax-e #'new-id)))
                            (orig-id (format-id #'new-id "~a" (syntax-e #'n-id))))
                #'(let ((orig-id 15))
                    (displayln 'n-id)
                    nnew-id)))))))))  ;;THIS WORKS!!! DON"T FORGET

(define-syntax (t6 stx)
  (syntax-parse stx
    ((_t6 name id)

     #'(define-syntax (name stx)
         (syntax-parse stx
           ((_name expr)
            (with-syntax ((n-id (format-id #'expr "~a" (syntax-e #'id))))
              #'(let ((n-id 77))
                  expr))))))))

(define q 56)


;;OK this works too, maybe it wasn't so hard...

;; BUT NOW, how to update the ENV as it changes?
;; Could have macro call 'get-all' to get ENV from closure then make the temp ENV to craft in...


; how about a macro that takes a quoted body and produces a proc?

#;
(define-syntax (mk-proc stx)
  (syntax-parse stx
    ((_mk-proc id)
     (define 
     (define args (second id))
     (define body (third id))
     (with-syntax ((arg-syntax args)
                   (body-syntax body))
       #'(lambda arg-syntax
           body-syntax))))))
