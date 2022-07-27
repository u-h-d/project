#lang racket

(require (for-syntax syntax/parse racket/syntax))

;; transformers to make entities ->

;; entity will have a controller (initially empty shell) that maybe has a translation layer to strip cmd fluff for
;; clean input/output rendering
;; controller is a 'flat' machine, meaning its just one layer deep, but we can still have hooks
;; will also have components which are just other entities
;; form will capture input and output and have set of hooks to run on each (draw, error handling, timing, continuations, etc)
;; components will be stored in hash (can have singles OR sets)
;; will need to have a way to print entity tree...
;; need setters/getters for manipulating/installing components
;; can intialize empty or with args
;; components will be name/value pairs, where a value could be a list of name/value pairs
;; could have it so empty components have some default value so you can "run" things w/out full completion
;; so, do we ahve to specify that we have input/output capture? or can that be optional?
; define our setters/getters
; called in creation of entity with current component hash

;; !!CURRENT PROBLEM: having a way to compose controllers independently using the names of components ->
;; ie, scope of identifiers is a challenge

(define-syntax (mk-set/get stx)
  
  (syntax-parse stx
    ((_mk-set/get name)
     (with-syntax ((set-id (format-id #'name "~a-set" (syntax-e #'name)))
                   (get-id (format-id #'name "~a-get" (syntax-e #'name)))
                   (get-all-id (format-id #'name "~a-get-all" (syntax-e #'name)))
                   (run-id (format-id #'name "~a-run" (syntax-e #'name))))

       #'(begin
           (define-syntax-rule (set-id id val)
             (name `(set id ,val)))
           (define-syntax-rule (get-all-id)
             (name '(get-all)))
           (define-syntax-rule (get-id id)
             (name '(get id)))
           (define-syntax-rule (run-id val)
             (name '(run val))))))))
     

     
; run-hook procs
; we assume each hook acts independently..
(define (run-hooks lst-hooks val)
  (for ((hook lst-hooks))
    (hook val)))
(define (comp-print comp-hash)
  comp-hash)

;; initiate a plain entity
(define-syntax (mk-entity stx)

  (syntax-parse stx
    
    ((_mk-entity name comp-pairs)
     ; mk new id's for each component/sub-component and assoc w/ value
     (with-syntax ((((ids vals) ...)  #'comp-pairs)
                   (hidden-name (format-id #'name "~a" (gensym))))
     
       #'(begin
           ; helper macros
           (mk-set/get name)
           #;(mk-wkshp name)
           ; define our entity as a closure
           (define name
             (let ((components (make-hash `((controller .
                                            ,(lambda (input)
                                              'controller))
                                           (input-hooks . ())
                                           (output-hooks . ()))))) ; this is where we have to insert init args if any

               
               
               (hash-set! components 'ids vals) ...
               ; helper procs
               #;
               (define-syntax (new-contr stx)
                 (syntax-parse stx
                   ((_new-contr body-expr)
                    (with-syntax ((body-syntax (datum->syntax #f #'body-expr)))
                      #'(lambda (cmd)
                          body-syntax)))))
                   
               
               ; core dispatch 
               (lambda (cmd)
                 (case (car cmd)
                   ((set) (if (eq? 'controller (second cmd))
                              (hash-set! components (second cmd) (third cmd))
                              ;(new-contr (third cmd))
                              (hash-set! components (second cmd) (third cmd))))
                   ((get) (hash-ref components (second cmd)))
                   ((get-all) (comp-print components))
                   
                   ; main function
                   (else
                    (begin
                      (displayln "here we are")
                      (run-hooks (hash-ref components 'input-hooks) cmd) ; this might be where translation layer comes in
                      (define output
                        ((hash-ref components 'controller) cmd)) ; want to run on naked 'cmd' bc that is the actual format expecte by program
                      (run-hooks (hash-ref components 'output-hooks) output))))))))))))
     




(mk-entity test ((a 4) (b 3)))

; SO, could there be some hooks that come from above (maybe a wrapping drawing/time mgr) or from
; outside (logging data)



#;; won't work bc not in scope of 'components'
(define c1
  (lambda (cmd)
    (+
     (hash-ref components 'a)
     (hash-ref components 'b))))

(define-syntax (defcontr stx)

  (syntax-parse stx
    ((_defcontr name (comps ...) body)
     (with-syntax ((cmd-id (format-id #'name "~a" (datum->syntax #'name 'cmd))))
     #'(define (name cmd-id comps ...)
         body)))))
     
