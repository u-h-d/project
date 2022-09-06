#lang racket

(require racket/stxparam (for-syntax syntax/parse racket/syntax))


(provide (all-defined-out))

;; THOUGHT, consider a functionality like a call-back (that we could represent visually) where
;; what a controller produces is a function that re-calls the producing controller...

;;; ************************************************************************************************************************************
;;; MK-ENTITY **************************************************************************************************************************
;;; ************************************************************************************************************************************
; !! FUUTURE STEP -> making 1) type declarations part of init 2) setting default controllers on init that install components...
; worth noting we can't really change the name...
(define (mk-entity sym)
  ; closure
  (let ((name sym)
        (components (make-hash
                     '(#;(a . 4)
                       #;(b . 5))))
        (controller  (lambda (cmd)
                       'blank-controller))
        (agents '())
        (spawn-list '())
        (mutate-hooks '())
        (ihooks '())
        (ohooks '()))

    ; meta-dispatch
    (lambda (cmd)
      (printf "CMD coming in to ~a is ~a\n" name cmd)
      (define meta-cmd (car cmd))
      (define payload (cdr cmd))
      (displayln payload)
      (case meta-cmd
        
        ((set) (hash-set! components (car payload) (cadr payload)))
        ((set-name) (set! name (car payload))) 
        ((set-input-hook) (set! ihooks (cons (car payload) ihooks)))
        ((set-output-hook) (set! ohooks (cons (car payload) ohooks)))
        ((set-mutate-hook) (set! mutate-hooks (cons (car payload) mutate-hooks)))
        ((set-controller) (set! controller (car payload)))
        ((set-agent) (set! agents (cons (car payload) agents)))
        ((add-spawn) (set! spawn-list (cons (car payload) spawn-list)))
        ((get-all) components)
        ((get) (hash-ref components (car payload)))
        ((get-controller) controller)
        ((get-name) name)
        ((get-hooks) `(,ihooks ,ohooks))
        ((get-ihooks) ihooks)
        ((get-ohooks) ohooks)
        ((get-agents) agents)

        ; run the entity via controller
        ; another option is to have this be an else so that its just (name arg) to invoke a "running" of the entity
        ; next problem is that if controller is composed like (subc2 (subc1 arg)) then what subc2 gets is not properly formed as `(run ,arg)
        ; previous idea of having 'run' be catch-all may work, just check for pair? at top, if so check for hits, no hits comes here,
        ; OR, if not a pair, comes here
        ((run)
         (let ((arg (car payload))) ; strip the outer list off...
           (displayln "entering 'run'")
           (run-hooks ihooks arg)
           (define result (controller `(run ,arg)))
           (run-hooks ohooks result)
           result))))))

;; define a set of transformers to streamline (at least for interactive use)
;; notice these use 'pvt-name' -> not sure if necessary
(define-syntax (mk-e/interface stx)
  
  (syntax-parse stx
    ((_mk-interface e-name)

     #:with pvt-name (format-id #'e-name "~a/~a" (syntax-e #'e-name) (gensym))

     ; to avoid a define, could you use a 'let' here?  have to make an id for let form?
     ; problem with 'run' macro is that whatever the arg is gets stuck in a list ->
     ; when we pull as 'payload', now have (list arg), and when we do subsequent 'run' macros, THAT is stuck in list
     ; now doubly nested... -> could set-up defctlr to auto insert (define n-cmd (car cmd)) so keeps stripping off...?
     ; seems clunky.  other alternative is to work out defctlr so it doesn't use macro calls but just invokes procs...
     #'(begin
         (define pvt-name (mk-entity 'e-name))
         (define-syntax e-name
           (syntax-rules (set-controller set set-name set-agent set-mutate-hook get get-all get-controller get-name get-hooks run)
             ((e-name set-controller arg) (pvt-name `(set-controller ,arg)))
             ((e-name set key arg) (pvt-name `(set key ,arg)))
             ((e-name set-name name) (pvt-name `(set-name name)))
             ((e-name set-agent agent) (pvt-name `(set-agent ,agent)))
             ((e-name set-mutate-hook hook) (pvt-name `(set-mutate-hook ,hook)))
             ((e-name add-spawn spawn) (pvt-name `(add-spawn ,spawn)))
             ((e-name get-pvt) pvt-name)
             ((e-name get-all) (pvt-name '(get-all)))
             ((e-name get key) (begin
                                 (displayln "match")
                                 (pvt-name '(get key))))
             ((e-name get-controller) (pvt-name `(get-controller)))
             ((e-name get-name) (pvt-name '(get-name)))
             ((e-name get-hooks) (pvt-name '(get-hooks)))
             ((e-name get-agents) (pvt-name '(get-agents)))
             ((e-name run arg) (pvt-name `(run ,arg)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;**MAKE-ALL**
;;;;;;;;;;;;;;

; this is used to initiate a build by making the head first, then calling helper macro to do the rest
; this was necessary bc initial version made subs, then on recur REMADE w/ same symbol this time as HEAD
(define-syntax-rule (mk-all (head rest ...))
  (begin
    (mk-e/interface head)
    (mk-rest (head rest ...))))

;; MK-REST
;; init a framework of symbols into entities
; mk new and pass w/ subs -> for each sub, install result of recurring w/ mk new and ITS subs -> when no subs, return
(define-syntax (mk-rest stx)
  (syntax-parse stx
    ((_mk-all (name (sub more ...) ...))

     #'(begin
         ; (mk-e/interface name)
         (mk-e/interface sub) ...
         (name set sub (sub get)) ...
         (mk-rest (sub more ...)) ...))))


;;; ***********************************************************************************************************************************
;;; UTILITIES **************************************************************************************************************************
;;; ************************************************************************************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;**DEFCTLR**
;;;;;;;;;;;;;;

; param to link macro introduced 'cmd-id', which expands to 'cmd', to uses of 'cmd' in syntax brought in by macro USE
(define-syntax-parameter ctl-cmd
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "only used inside defctlr")))

;; takes the id of a entity, a list of needed components and a procedure body ->
;; make a set of let bindings where each components value is obtained by looking it up in entity 'id' hash
;; stick the given body an expression inside the dynamic context of the 'let'
;; let has to be INSIDE a lambda so it updates when referenced entity is mutated
;; probably need an error if given comps NOT found in entity closure
;; right now, a simple 'set-controller' works bc if use macro interface calls in body, works w/out dynamic binding
(define-syntax (defctlr stx)

  (syntax-parse stx
    ((_defctlr name (comp ...) ((~literal step) num step-expr) ...((~literal result) result-expr))
     
     #:with cmd-id (format-id #'name "~a" 'cmd)
     #:with arg-id (format-id #'name "~a" 'arg)
     #:with ctlr-id (format-id #'name "~a-~a" (syntax-e #'name) 'ctlr)
     #:with comp-id (format-id #'name "~a" 'components)
     #:with result-hook-id (format-id #'name "~a" 'result)

     ; turn our matched 'comp's into a list 
     (define components (syntax->list #'(comp ...)))
     (define ctlr-comp (syntax->list #'(num ...)))
     (define steps (syntax->list #'(step-expr ...)))
     (displayln steps)
     ; have to make new bindings w/ proper context so they capture same ids that are brought in in the 'body'
     (define let-bindings
       #`(
         #,@(for/list ((c components))
           (with-syntax ((comp-id (format-id #'name "~a" (syntax-e c))))
             #'(comp-id ((comp-id get) cmd-id))))
         #,@(for/list ((c ctlr-comp))
           (with-syntax ((comp-id (format-id #'name "~a" (syntax-e c))))
             #'(comp-id (ctlr-id get comp-id))))
         #,#'(result-hook-id (ctlr-id get result-hook-id))))

     (define step-bindings
       (for/list ((c ctlr-comp)
                  (s steps))
         
         (with-syntax ((r-id (format-id #'name "r-~a" (syntax-e c))))
           #`(define r-id (#,c #,s))))) 
     
     ; NOW, need the construction of a controller to be an entity that IS the controller ->
     ; the nested entities controller will be a lambda, and its expression will be the one given to the original constructor
     ; the components of the controller entity will be those elements we wish to capture
     ; what is a ctlr component exactly? -> is it just an entity w/ no components/controller, and only hooks? maybe just
     ; single hook?
     ; the ctlr needs to be full entity, its components simplified (as above)
     ; several situations -> higher component is a value, or it is a function that performs ->
     ; but still is called just the same
     ; we define what happens in our expression, so we don't need REAL compnonents, just need to represent what's happening
     ; r1 would be the result of (+ 1 a) called with that value, and what r1 does is simply provide hook access to that result
     ; what is r1's draw-space? ->
     (displayln (syntax->datum let-bindings))
     (displayln step-bindings)
     ; OK, NEED TO SORT OUT ID SITUATION FOR SETTING ALL CTLR COMPONENTS
     #`(begin
         ; mk new entity which is 'name's controller ->
         (mk-e/interface ctlr-id)
         ; for every 'step' provided, set as a component in ctlr entity with simple lambda for hooks
         ;#,@(for/list ((c ctlr-comps))
              ;#:with n-id (format-id #'name "~a" (syntax-e #'c)))
         (ctlr-id set num (let ((step-number (symbol->string 'num))
                                (hooks '()))
                            (lambda (result)
                              (printf "Step ~a produced ~a\n" step-number result) 
                              (run-hooks hooks result)
                              result))) ...
         ; set component (hook-holder) for result
         (ctlr-id set result-hook-id (let ((hooks '()))
                                       (lambda (result)
                                         (run-hooks hooks result)
                                         (printf "Result is: ~a\n" result))))
         ; 'ctlr' entity's controller will be simple lambda ->
         (ctlr-id set-controller
                  ; evaluate our expressions in dynamic context.  we splice together a body ->
                  (lambda (cmd-id)
                    (displayln "coming into ctlr-ctlr")
                    ;(displayln (ctlr-id get one))
                    ;(displayln 'step-expr) ...
                    (let* #,let-bindings
                      (syntax-parameterize ((ctl-cmd (make-rename-transformer  #'cmd-id))) ; do this to link cmd-id to 'cmd' in body
                        (begin
                          (displayln "starting expr eval")
                          #,@step-bindings
                          (result-hook-id result-expr))))))
     ; finally, set 'name's controller to be what we have created in 'ctlr-id' entity
     (name set-controller (ctlr-id get))))))

      


;; I suppose you could have provide entire lambda form, and insert the 'let', then we wouldn't need to worry about 'cmd-id'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;**HOOKS**
;;;;;;;;;;;;

;;; <HOOK datatype>
; we let hook be a struct so we can have meta-data beyond proc (could use closure, too, but maybe more cumbersome w/ message dispatch)
(struct hook (name tags proc) #:mutable)

;;; RUN-HOOKS
;;; called anytime an entity receives a 'run' cmd -> passes payload through each hook proc in turn
(define (run-hooks hooks input)
  (for ((hook hooks))
    (let ((proc (hook-proc hook)))
      (proc input))))

;;; INSTALL-HOOK
;;; uses higher-order 'crawl-e' to walk the entity, initiating the proto-hook each template carries, then building
;;; hook-struct w/it and rest of template
(define (install-hook hook-template e)
  (crawl-e e (lambda (e)
               (define init-hook ((caddr hook-template) e))
               (define hook-struct (hook
                                    (car hook-template)
                                    (cadr hook-template)
                                    init-hook))
               (e `(set-input-hook ,hook-struct)))))



;;;**END HOOKS**
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;**ENTITY WALKER**

; this is depth first.. should we also have breadth first?
(define (crawl-e e proc)
  (begin
    (proc e)
    (let ((comps (hash-keys (e '(get-all)))))
      (if (null? comps)
          'DONE
          (begin
            ;(displayln comps)
            (for ((comp comps))
              (let ((nxt-e (e `(get ,comp))))
                (crawl-e nxt-e proc))))))))

; macro for smoother call to 'crawl-e', which requires a proc
(define-syntax-rule (crawl name proc)
  (crawl-e (name get) proc))

;;; ^ENTITY WALKER^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;**PRINT UTILITIES**

;;; PRINT-E
;;; print a semi-readable tree showing an entity and all its subs
;;; can this be simplified w/ 'crawl-e'?
(define (print-e e)

  (define (p-rec parent level)
    (define padding (make-string level #\ ))
    ; start w/ empty str and fold across subs making a set of padded strs (with \n where needed)
    (for/fold ((str ""))
              ((comp-key (hash-keys (parent '(get-all)))))
      (string-append str padding (format "~a <input-hooks> ~a  <output-hooks> ~a\n~a"
                                         comp-key
                                         (parent '(get-ihooks))
                                         (parent '(get-ohooks))
                                         (p-rec (parent `(get ,comp-key)) (+ 1 level))))))
  ; init
  (printf "~a\n~a" (e '(get-name)) (p-rec e 1)))

(define-syntax-rule (print-entity e)
  (print-e (e get)))


;;; ***********************************************************************************************************************************
;;; TESTING ***************************************************************************************************************************
;;; ***********************************************************************************************************************************

; a hook template for a simple hook
(define basic
  `(
    ; name
    'basic
    ; tags/flags/etc
    ()
    ; proto-proc (could abstract this part)
    ,(lambda (entity)
       (let ((name (entity '(get-name))))
         (lambda (arg)
           (printf "~a's basic hook is passing through: ~a\n" name arg))))))

; example entity tree
; consider name change to something like 'entity' so it reads more like (struct ...) -> should handle single as well?
(mk-all
 (zh4
  (subc1 (dd1 (sdd6)) (dd2))
  (subc2 (dd3))
  (subc3 (dd4 (sdd7)))))

(install-hook basic (zh4 get))

(mk-all
 (node
  (a)
  (b)))

(install-hook basic (node get))

(a set-controller (lambda (cmd)
                    4))
(b set-controller (lambda (cmd)
                    5))

#;(defctlr node (a b)
  (step one (+ 1 a))
  (step two (+ 2 b))
  (result (+ r-one r-two)))

;;;;;;; DRAW STUFF ->
#|

(define (mk-spawn-id m-name e-name)
  (let ((m (symbol->string m-name))
        (e (symbol->string e-name)))
    (string->symbol (string-append e "-" m))))

(define (mk-mutate-hook-id name)
  (string->symbol (string-append (symbol->string name)-"mutate-hook")))

(mk-e/interface draw-mgr)

(draw-mgr set mk-spawn (lambda (entity)
                           (define name (mk-spawn-id (draw-mgr get-name) (entity '(get-name))))
                           (begin
                             (define agent
                               (lambda (cmd)
                                 (define e entity)
                                 'this-is-draw-agent))
                             (entity `(set-agent ,name ,agent))
                             (entity `(set-mutate-hook ,(mk-mutate-hook-id name)
                                                       ,(lambda (data)
                                                          (agent data)))) ; calling above defined agent w/ update info it will handle
                             (draw-mgr add-spawn (cons `(,name ,agent))))))


(draw-mgr set handler (lambda (cmd)
                        (case (car cmd)
                          ((install) install))))
(draw-mgr set install (lambda (entity)
                        (crawl entity mk-spawn)))
                              
(defctlr draw-mgr (mk-spawn
                   install
                   handler)
  (result handler))
                             
  

  ; thinking about draw-mgr controllers (remember the controller is itself an entity (flat?) ->
  ; needs to loop and draw on tick but also handle update cmds...
  #| 
  
  1) have proc for installing new agents (component)
  2) re-install agents when notified a comp has been replaced
  3) have list of draw procs provided by agents (component)
  4) properly update agents "file" or draw proc -> ie, we maintian an assoc of each agent in the field and its current draw proc
  5) alert agents of space change (top down)

  |#

  ; this is an entity...
  #;(let ((name "enclosing-entity-name/contr")
        (install (lambda (entity)
                   'install-entity))
        (agent-data `((agent1 ,a1-proc)
                      (agent2 ,a2-proc)))
        
        (lambda (cmd)

          (if cmd
              (case (car cmd)
                ; we are being alerted an entity in our care has changed
                ; not sure what the msg would say exactly..
                ; presumably it'll give us its name or proc or something..
                ((entity-update) (deal-w-change (cadr msg)))
                ((install) (install (cadr msg)))
                ((start)
                 (begin
                   (define draw-thread (mk-draw-thread)) ; mk-draw-thread would be another component
                   (hash-set! 'draw-thread draw-thread)
                   (draw-thread)))
                ((stop)
                 (if (hash-ref components 'draw-thread)
                     (kill-thread (hash-ref components 'draw-thread))
                     'NO-THREAD)))))))

  ; example install proc ->

    
        
  ; thinking about draw-agent controller ->

|#  

