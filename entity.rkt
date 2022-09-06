#lang racket

(require racket/stxparam (for-syntax syntax/parse racket/syntax))


(provide (all-defined-out))

;; THOUGHT, consider a functionality like a call-back (that we could represent visually) where
;; what a controller produces is a function that re-calls the producing controller...

;;; ************************************************************************************************************************************
;;; MK-ENTITY **************************************************************************************************************************
;;; ************************************************************************************************************************************

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
        (mutate-alert-list '())
        (ihooks '())
        (ohooks '()))

    ; meta-dispatch
    (lambda (cmd)
      (printf "CMD coming in to ~a is ~a\n" name cmd)
      (define meta-cmd (car cmd))
      (define payload (cdr cmd))

      (case meta-cmd
       
        ((set) (hash-set! components (car payload) (cadr payload)))
        ((set-name) (set! name (car payload))) 
        ((set-input-hook) (set! ihooks (cons (car payload) ihooks)))
        ((set-output-hook) (set! ohooks (cons (car payload) ohooks)))
        ((set-controller) (set! controller (car payload)))
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
           (define result (controller arg))
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
           (syntax-rules (set-controller set set-agent set-mutate-hook add-spawn set-name get get-all get-controller get-name get-hooks run)
#|
             ((e-name set-controller arg) (pvt-name `(set-controller ,arg)))
             ((e-name set key arg) (pvt-name `(set key ,arg)))
             ((e-name set-name name) (pvt-name `(set-name name)))
             ((e-name get) pvt-name)
             ((e-name get-all) (pvt-name '(get-all)))
             ((e-name get key) (pvt-name '(get key)))
             ((e-name get-controller) (pvt-name `(get-controller)))
             ((e-name get-name) (pvt-name '(get-name)))
             ((e-name get-hooks) (pvt-name '(get-hooks)))
             ((e-name get-agents) (pvt-name '(get-agents)))
             ((e-name run arg) (pvt-name `(run ,arg)))))))))
  |#
             
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
    (raise-syntax-error (syntax-e stx) "only used inside deftclr")))

;; takes the id of a entity, a list of needed components and a procedure body ->
;; make a set of let bindings where each components value is obtained by looking it up in entity 'id' hash
;; stick the given body an expression inside the dynamic context of the 'let'
;; let has to be INSIDE a lambda so it updates when referenced entity is mutated
;; probably need an error if given comps NOT found in entity closure
;; right now, a simple 'set-controller' works bc if use macro interface calls in body, works w/out dynamic binding
(define-syntax (defctlr stx)
  
  (syntax-parse stx
    ((_defctlr name (comp ...) body)
     
     #:with cmd-id (format-id #'name "~a" 'cmd)
     #:with arg-id (format-id #'name "~a" 'arg)
     
     (define components (syntax->list #'(comp ...)))

     ; have to make new bindings w/ proper context so they capture same ids that are brought in in the 'body'
     (define let-bindings
       (cons #'(arg-id `(run ,cmd-id)) ; anytime we see 'arg' id in body, we dynamically assign to `(run ,cmd-id)
             (for/list ((c components))
               (with-syntax ((comp-id (format-id #'name "~a" (syntax-e c))))
                 #'(comp-id (comp-id get))))))
     ; issue is that expands to where (subc1 cmd) becomes ((subc1 get) run cmd) and then 'run' is NOT used in macro and thus
     ; complains that it is undefined..
     (displayln let-bindings)
     #`(let ((ctlr 
              (lambda (cmd-id) ; cmd-id had to be made bc any identifier we introduce has to be properly made
                (let #,let-bindings
                    (syntax-parameterize ((ctl-cmd (make-rename-transformer  #'cmd-id))) ; do this to link cmd-id to 'cmd' in body
                      body)))))
         (name set-controller ctlr))))) ; this is infact a macro call... hope works

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

(defctlr subc1 (dd1) `(,(dd1 arg)))
(defctlr subc2 (dd3) `(,(dd3 arg)))
(defctlr subc3 (dd4) `(,(dd4 arg)))
(defctlr zh4 (subc1 subc2 subc3)
  (begin
    (define r1 (subc1 arg))
    (define r2 (subc2 arg))
    (subc3 arg)))
