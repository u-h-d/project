#lang racket

(require (for-syntax syntax/parse))

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
(define-syntax (mk-set/get stx)
  
  (syntax-parse stx
    ((_mk-set/get id ht)
     
     #'(begin
         (define-syntax-rule (eset id val)
           (hash-set! ht 'id val))
         (define-syntax-rule (eget id)
           (hash-ref ht 'id))))))
              
(define-syntax (mk-entity stx)

  (syntax-parse stx
    ((_mk-entity name components)
     ; mk new id's for each component/sub-component and assoc w/ value
     (with-syntax (((ids vals) ...  #'components))
     
       #`(begin
           (define ht (make-hash)) ; not sure this will work...
           ; define our entity as a closure
           (define name
             (let ((components ht))) ; this is where we have to insert init args if any
             ; can i stick the set/get here? 
             (lambda (cmd)
               (define input (translate cmd)) ; this might be where translation layer comes in
               (define output
                 ((hash-ref components 'controller) cmd)) ; want to run on naked 'cmd' bc that is the actual format expecte by program
               (run-output-hooks output))) ; could have his be a form that has as its return just 'output'

           #,@(map mk-set/get
                   ids))))))
       
           






                 
