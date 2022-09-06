#lang racket

(require (for-syntax syntax/parse racket/syntax))


(module above racket

  (require racket/stxparam (for-syntax syntax/parse racket/syntax))

  (provide (all-defined-out))

  (define-syntax (mk-cc stx)
    (syntax-parse stx
      ((_mk-cc name id)

       #:with n-id (format-id #'name "~a/cc-~a" (syntax-e #'name) (syntax-e #'id))

       #'(define-syntax n-id
           (lambda (stx)
             (syntax-parse stx
               (_n-id
                #'(name get id))))))))

  (define-syntax-parameter cmd
    (lambda (stx)
      (raise-syntax-error (syntax-e stx) "only used inside deftclr")))

  ; substitute any 'comp' found in body w/ corresonding value in hash
  (define-syntax (defctlr stx)
    (syntax-parse stx
      ((_defctlr name (comp ...) body)
       (define components (syntax->list #'(comp ...)))
       (define let-bindings
         (for/list ((c components))
           (with-syntax ((comp-id (format-id #'name "~a" (syntax-e c)))
                         (cmd-id (format-id #'name "~a" 'cmd))) ; remember, this an id'r we'll parameterize to match any 'cmd' in body
             (displayln c)
             #'(comp-id (name get comp-id)))))
       
       #`(let ((ctlr 
                (lambda (cmd-id) ; cmd-id had to be made bc any identifier we introduce has to be properly made
                  (let #,let-bindings
                      (syntax-parameterize ((cmd (make-rename-transformer  #'cmd-id))) ; do this to link cmd-id to uses of 'cmd' in body
                        body)))))
           (name set-controller ctlr)))))
                  
                     

  (define-syntax (mk-entity stx)
    (syntax-parse stx
      ((_mk-entity name)

       #:with pvt-name (format-id #'name "~a/~a" (syntax-e #'name) (gensym))

       #'(begin
           
           (mk-cc name a) ; will work here 
           
           (define pvt-name

             (let ((components (make-hash  ;!!! something worth noting -> trying to pull datum expr out of hash w/ 'hash-ref'
                                `((a . ,(lambda ()
                                         'this-a))  ;!!! caused it to try to eval or something and complain of missing identifiers...?
                                  (b . ,(lambda ()
                                         'this-b)))))
                   (controller  '(list (a) (b))))

               #;(mk-cc name a) ; id hidden here
               
               (lambda (cmd)

                 (displayln cmd)
                 (case (car cmd)
                   ((set) (hash-set! components (second cmd) (third cmd)))
                   ((set-controller) (set! controller (second cmd)))
                   ((get) (hash-ref components (second cmd)))
                   ((get-controller) controller)
                   ; build an expression then 'eval'
                   ((run)
                    (eval
                     `(let ,(for/list ((key (hash-keys components)))
                              `(,key ,(hash-ref components key)))
                          ,controller)))
                   ; 
                   ((run2) (controller (cadr cmd)))))))

           (define-syntax name 
             (syntax-rules (set-controller set get get-controller run run2)
               ((name set-controller arg) (pvt-name `(set-controller ,arg)))
               ((name set key arg) (pvt-name `(set key ,arg)))
               ((name get key) (pvt-name `(get key)))
               ((name get-controller) (pvt-name `(get-controller)))
               ((name run arg) (pvt-name `(run ,arg)))
               ((name run2 arg) (pvt-name `(run2 ,arg))))))))))
       

(require 'above
         (for-syntax 'above))

            

(mk-entity test)
