#lang racket

(require racket/syntax
         (for-syntax syntax/parse racket/syntax))
(provide (all-defined-out))

(define l 10)
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

;; !!module w/ tree defined and list of bindings for tree names, init to null ->
;; require this into module where drafting can occur and then update those values if desired
;; can have multiple draft modules going...

;; !!CURRENT PROBLEM: having a way to compose controllers independently using the names of components ->
;; ie, scope of identifiers is a challenge
(module above racket
  
  (require (for-syntax syntax/parse racket/syntax))

  (provide (all-defined-out) ) ; doing this provides any ID's made w/ macros... is there a better way?

  (define-syntax (define/contr stx)
    (syntax-parse stx
      ((_define/contr name body)

      ; #:with datum (syntax->datum #'body)

       #'(define name
           (list 'body
                 (lambda (arg)
                   body))))))
  
  (define-syntax (mk-set/get stx)
    
    (syntax-parse stx
      ((_mk-set/get name)
       (with-syntax ((set-id (format-id #'name "~a-set" (syntax-e #'name)))
                     (get-id (format-id #'name "~a-get" (syntax-e #'name)))
                     (get-all-id (format-id #'name "~a-get-all" (syntax-e #'name)))
                     (run-id (format-id #'name "~a-run" (syntax-e #'name)))
                     (print-id (format-id #'name "~a-print" (syntax-e #'name))))

         #'(begin
             (define-syntax-rule (set-id id val)
               (name `(set id ,val)))
             (define-syntax-rule (get-all-id)
               (name '(get-all)))
             (define-syntax-rule (get-id id)
               (name '(get id)))
             (define-syntax print-id
               (syntax-rules ()
                 ((print-id) (name '(print)))))
             (define-syntax run-id
               (syntax-rules ()
                 ((run-id) (name '(run #f)))
                 ((run-id val) (name '(run val))))))))))


  ; helper functions
  (define (get-bindings ht)
    (define ids
      (for/list ((entry (hash->list ht)))
        (car entry)))
    (define vals
      (for/list ((entry (hash->list ht)))
        (cdr entry)))
    (values
     ids
     vals))
  (define in-debug
    (lambda (arg)
      (printf "Input is ~a\n" arg)))
  (define out-debug
    (lambda (arg)
      (printf "Output is ~a\n" arg)))
  (define wait
    (lambda (arg)
      (sleep 1)))
  #;
  (define-syntax (mk-i/o-debug-hooks stx)
    (syntax-parse stx
      ((_ name)
       #:with set-id (format-id #'name "~a-set" (syntax-e #'name))
       
       #'(begin
           (set-id add-input-hook
                   (lambda (arg)
                     (printf "Input is ~a\n" arg)))
           (set-id add-output-hook
                   (lambda (arg)
                     (printf "Output is ~a\n" arg)))))))

  
  ; run-hook procs
  ; we assume each hook acts independently..
  (define (run-hooks lst-hooks val)
    (if (null? lst-hooks)
        (void)
        (begin
          (for ((hook lst-hooks))
            (hook val)))))
  (define (comp-print comp-hash)
    (map (lambda (entry)
           (let ((id (car entry))
                 (val (cdr entry)))
             (begin
               (printf "~a:\t\t~a\n" id val)
               comp-hash)))
         (hash->list comp-hash)))
  
  ; problem is init with no values for components, so no proc to find in hash table,
  ; BUT, the entity exists and we can get its hash...
  (define (e-print ht)
              
    (define (print-r ht layer)

      (displayln (hash-ref ht 'e-name))

      (define (print-init-all lst-keys layer)
        (for/fold ((acc-str ""))
                  ((key lst-keys))
          (let ((proc (hash-ref ht key)))
            (string-append acc-str
                           (print-r (proc '(get-all))
                                    layer)))))

      (let* ((keys (filter (lambda (key)
                             (not (or (eq? 'input-hooks key)
                                      (eq? 'output-hooks key)
                                      (eq? 'controller key)
                                      (eq? 'controller-datum key)
                                      (eq? 'e-name key))))
                           (hash-keys ht)))
             (name (hash-ref ht 'e-name))
             (name-str (format "~a~a\n" (make-string layer #\ ) name)))
        
        (if (null? keys)
            name-str
              (format "~a~a"
                      name-str
                      (print-init-all keys (+ 1 layer))))))
            
    (print-r ht 0))

  #;; probably crazy attemp here...
  (define-syntax (mk-print stx)
    (syntax-parse stx
      ((_mk-print name)
       
       (with-syntax ((print-id (format-id #'name "~a-print" (syntax-e #'name)))
                     (get-all-id (format-id #'name "~a-get-all" (syntax-e #'name))))
         
         #'(define-syntax-rule (print-id)
             (let* ((keys (hash-keys (get-all-id)))
                    (key-syntax
                   (syntax-parse #'keys
                     ((key (... ...))
                      #'(list (format-id #'name "~a-print" (syntax-e key)) (... ...))))))
               (printf "~a\n  " 'name)
             (for ((key key-syntax))
               (key))))))))

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
      
  #;
  (define-syntax (print-e stx)
    (syntax-parse stx
      ((_print-e name)

       #:with get-all-id (format-id #'name "~a-get-all" (syntax-e #'name))
       
       #'(begin
           (name '(print))
           (let ((ids (id-filter (get-all-id))))
             (p-all ids))))))
#;
  (define-syntax (p-all stx)
    (syntax-parse stx
      ((_p-all (id-datum ...))

       #:with (n-id ...) (map (lambda (datum)
                                (format-id #'_p-all "~a" datum))
                              #'(id-datum ...))
       #'(begin
           (print-e n-id) ...))))

 
    
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
             ;(component-call name ids) ...
             ;(mk-i/o-debug-hooks name)
             
             ; define our entity as a closure
             ; might want a way to dump expr that makes controller...
             (define name
               
               (let* ((components (make-hash `((e-name . name)
                                               (controller .
                                                           ,(lambda (input)
                                                              'controller))
                                               (controller-datum . #f)
                                               (input-hooks . (,in-debug ,wait))
                                               (output-hooks . (,out-debug ,wait))
                                              #;(sig-in . (car sig))
                                              #;(sig-out . (cadr sig)))))
                      #;(id-syntax (map (lambda (entry)
                                        (format-id #f "~a" (syntax-e (car entry))))
                                      (hash->list components)))) ; this is where we have to insert init args if any

                 ; would it be possible to have simple expressions in hook list that are eval'd in this closure?
                 
                 (hash-set! components 'ids vals) ...
                 
                 ; helper procs

                 #; ; maybe use later
                 (define-syntax (new-contr stx)
                   (syntax-parse stx
                     ((_new-contr body-expr)
                      (with-syntax ((body-syntax (datum->syntax #f #'body-expr)))
                        #'(hash-set! components 'controller
                                     (lambda (cmd)
                                       body-syntax))))))
                 
                 
                 ; core dispatch 
                 (lambda (cmd)
                   (case (car cmd)
                     ((set)
                      (case (second cmd)
                        ((controller) (hash-set! components 'controller (third cmd)))
                                      ;(new-contr (third cmd)))
                         ((add-input-hook) (hash-set! components 'input-hooks
                                                      (cons (third cmd)
                                                            (hash-ref components 'input-hooks))))
                         ((add-output-hook) (hash-set! components 'output-hooks
                                                      (cons (third cmd)
                                                            (hash-ref components 'output-hooks))))
                         (else
                          (begin
                            (hash-set! components (second cmd) (third cmd))
                            #;(component-call (second cmd))))))
                     
                     ((get) (hash-ref components (second cmd)))
                     ((get-all) components)
                     ((print) (e-print components))
                     ((print-all) (comp-print components))
                     
                     ; main function
                     ; might want hooks to know about other things like 'name' etc...
                     ((run)
                      (begin
                        (printf "Starting ~a ...\n" 'name)
                        (run-hooks (hash-ref components 'input-hooks) cmd) ; this might be where translation layer comes in
                        (define output
                          ((hash-ref components 'controller) cmd)) ; want to run on naked 'cmd' bc that is the actual format expecte by program
                   
                        (run-hooks (hash-ref components 'output-hooks) output)
                        output)))))))))))


  ; probably read from file to get list of entities to make ->
  (mk-entity test ((a 4) (b 3)))
;  (mk-entity subn1 ((a 5) (b 7)))
 ; (mk-entity node ((a 4) (subn1 subn1)))
  
  )



;;; **********************************************************************************************************************************
;;; **********************************************************************************************************************************
;;; DRAFT LAYER MODULE



(require 'above
         (for-syntax 'above))

; have to decide at what phase entities are going to be MADE ->

; SO, could there be some hooks that come from above (maybe a wrapping drawing/time mgr) or from
; outside (logging data)
                   
(define-syntax (draft stx)
  (syntax-parse stx
    ((_draft name arg 
             (comp-id ...)
             expression)

     #:with get-all-id (format-id #'name "~a-get-all" (syntax-e #'name))
     #:with ht-id (format-id #'name "~a" 'ht)



     ; is it necessary to make updated id's when expr is supplied w/ macro...?
     ; problem is have to make ids at phase 1, but this will be called at phase 0
     ; could just have an assoc table...
     #`(let* ((ht (get-all-id))
              #,@(for/list ((id (syntax->list #'(comp-id ...))))
                    
                    (with-syntax ((n-id (format-id #'name "~a" (syntax-e id))))
                      #'(n-id (hash-ref ht 'n-id)))))
         ((lambda (payload)
            expression)
          arg)))))  ; WORKS!!


; BUT, could we have a way to get CURRENT component ID's from the hash-table?
; like maybe a function that returns SYNTAX?

; consider 'local-expand' and 'syntax-parameterize'

;; HOW ABOUT TESTING ->
;; need to be able to have a list of test cases and then have macro set them all up
;; could also maybe have a list of test EXPR (controllers) to test as well
;; maybe have this file be auto-drafted w/ templates supplied?

;; maybe there is a way to have inner module update some retrievable syntax that represents component ids?


#;(draft node #f
       (a subn1)
       (+ a
          (draft subn1 #f
                 (a b)
                 (+ a b)))) ; this works ...

; maybe should have this so any ref to a component expands into draft call w/ current controller ->
; if not defined, default value ...

; NEW PROBLEM -> when actually use controller, doesn't know about component bindings bc just in hash table

(define-syntax (contr-commit stx)
  (syntax-parse stx
    
    ((_contr-commit name (comp-ids ...) e)
     
     #:with call-id (format-id #'name "~a-run" (syntax-e #'name))
     #:with get-all-id (format-id #'name "~a-get-all" (syntax-e #'name))
     #:with set-id (format-id #'name "~a-set" (syntax-e #'name))
                        
     #'(begin
         (set-id controller (lambda (cmd)
                              e))
         (define-syntax (call-id stx)
           (syntax-parse stx
             ((_call-id arg)
              
              #`(let* ((ht (get-all-id))
                       #,@(for/list ((id (syntax->list #'(comp-ids ...))))
                            
                            (with-syntax ((n-id (format-id #'name "~a" (syntax-e id))))
                              #'(n-id (hash-ref ht 'n-id)))))
                  
                  ((lambda (cmd)
                     e)
                   arg)))))))))

; so this works... but we're getting away from intention here ->
; getting to where the closure just stores the data structure, and we extract the bindings and evaluate in a new context...
; might need a macro to rewrite to something like
#;((hash-ref (get-all) 'comp-name-symbol) arg)
; maybe make an id alias for (test-get a) = a in the body

; could use macros to set things bc can just pass in an expression and don't have to worry about lambda

(test-set controller
          (lambda (cmd)
            (+ (test-get a) (test-get b))))

#;(test-set add-input-hook
          (lambda (input)
            (printf "Input is: ~a\n" input)))
#;(test-set add-output-hook
           (lambda (output)
             (printf "Output is: ~a\n" output)))

(define p5
  (lambda (cmd)
    (displayln 'ANOTHER)))

(define (invoke proc)
  (test-set add-input-hook
            proc))

(mk-entity t2 ((c 8) (d 9)))

(t2-set controller
        (lambda (cmd)
          (+ (t2-get c) (t2-get d))))

(test-set controller (lambda (cmd)
                       (+ (- (test-get a)
                             (test-get b))
                          (t2 cmd))))

; contr need to be a flat entity -> it's lambda is the "brains" of the parents controller,
; but any component it has cannot nest deeper...

; IE, when the controller is an actual entity, not just a function, ITS controller MUST be a function ->
; though its COMPONENTS i guess could not be so restricted...?

; may have to make a CONTROLLER entity be a special case of entity
; or controller is just a lambda but have macro spruce it up...
; seems like if we want to hook, we should be using an entity...

; can we eventually have a separate process so we can make changes while machine is running?

(mk-entity node ())

(mk-entity graph-keeper ())
(mk-entity pathfinder ())
(mk-entity tx-maker ())
(mk-entity graph ())

(pathfinder-set controller (lambda (cmd)
                             'HERE-IS-PATH))
(graph-set controller (lambda (cmd)
                        'HERE-IS-GRAPH))
(graph-keeper-set pathfinder pathfinder)
(graph-keeper-set graph graph)
(graph-keeper-set controller (lambda (cmd)
                               (define g (graph cmd))
                               (define p (pathfinder '(g)))
                               p))
(node-set graph-keeper graph-keeper)
(tx-maker-set controller (lambda (cmd)
                           'HERE-IS-TX))
(node-set tx-maker tx-maker)
(node-set controller (lambda (cmd)
                       (define path (graph-keeper cmd))
                       (define tx (tx-maker cmd))
                       `(,path ,tx)))


 

; the above may not work like I want, may be accessing these functions from outside... could redo
; w/ macro where you tag id like (e-pathfinder cmd) which expands to ((hash-ref components 'pathfinder) cmd)

; SO, here we can pass a anon lambda w/ unbound ids bc its a macro...
; maybe for defaults, have tags that alert controllers so simply pass it through w/ a simple display (avoid errors which
; would be caused by normal behaviour w/ a non-standard filler argment

; !!ISSUE -> if we have, say, multiple nodes, how can we not conflict ids if components are the same? ->
; maybe have hidden unique ids for everything that is used internally? ->
; then have to have way to interactively work on them (maybe have command to switch between instances?)
; of course, if node calls 'graph-keeper' its relying on a closure, so no unique id needed ->
; but if I do (graph-keeper get graph), how does it know which node i want?
; might actually be good to have functionality where returns all instances of a common component for analysis...

; maybe should have in data struct (hash) a field for description of component
; OH, will hash ids have to match actual entity names...? -> NO
                            
                                 
#;(mk-entity zh (
               (concat (mk-entity concat ())) ; this is expanding into several 'define-syntax' which don't work in an 'expression' context
               (rev (mk-entity rev ()))
               (rand (mk-entity rand ()))
               ))


; will have to have DSL or another macro if want to describe nested entities in single expression ->

(mk-entity subz ())
(subz-set controller (lambda (cmd)
                       'SUB))
(mk-entity zh ((sym-return subz)))

; might need syntax class here...
; need it to also set! each component field to the corresponding entity...
(define-syntax (mk-all stx)

  (syntax-parse stx
    ((_mk-all name ((comp-id sub-comps) ...))
     #'(begin
         (mk-entity name ((comp-id #f) ...))
         (mk-all comp-id sub-comps) ...
         (name `(set comp-id ,comp-id)) ...))
     
    ((_mk-all name ())
     #'(mk-entity name ()))))
               

; an entity tree is:
; (mk-all id (components ...))
; where 'component is: (id (components)) OR (id ())

              
(mk-all zh4
        ((subc1 ((dd1 ())
                 (dd2 ())))
         
         (subc2 ((dd3 ())
                 (dd4 ((dd5 ())))))))
        
;; TODO -> parse tree, print tree, install pkg, flag system for inclusion/exclusion
;; !!remember the idea of having a) default values of entity/controllers, b) having tests that can come from above AND below baked into draft mode
;; where you have a macro that can quickly define a bunch of tests OR example inputs/outputs


; might have a function embedded that pulls on data inside entity and customizes hook therefrom
#;
(define (install-hook entity hook-data)
  ; hook-data ->
  ; in, out, or both
  ; build-proc?
  ; flags/tags?
  ; single, all, or how many layers
  ; hook-name?
  ; priority?
  ; does it adjust values as cascades down? (ie, draw-spaces) -> would need macro so you could pass lambda w/ unbound ids...

  ; need to present a data structure w/ all this info, and
  ; then have all the commands spawned to do it...
  (entity '(set-input-hook hook-proc)))

;; need way to insert component easily interactively -> need to make an entity which actualy does several definitions
;; as opposed to function which returns a value

(define-syntax (ins-comp stx)
  (syntax-parse stx
    ((_ins-comp name comp-name)
     
     
     
     #'(begin
         (mk-entity comp-name ())
         (name `(set comp-name
                     ,comp-name))))))


