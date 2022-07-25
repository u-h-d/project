#lang racket

(require (for-syntax racket/syntax racket/base syntax/parse))

(define y 4)
(define wait 10)
(define x 12)

; more macro learning
; maybe the top-level 'let' includes syntax transformers...?

(define-syntax-rule (intr x)
  (let ((y 4))
    x))

; don't work if you introduce 'x' as syntax to 'intr2' ->
; so, we have an 'x' scoped to the code of 'intr2', and an 'x' consumed by invocation w/ different scope
; if we have global 'x', that is what 'intr2' expansion code returns (inner x is not REAL 'x'
(define-syntax-rule (intr2 id)
  (let ((x 4))
    id))


; OK -> need to convert to syntax parameter and convert list of pairs
(define-syntax (intr3 stx)
  (syntax-parse stx
    ((_intr3 id)
     (with-syntax ((new-id (datum->syntax #'_intr3 'x)))
       #'(let ((new-id 4))
           id)))))

; a way to pass an env to a macro  ->
(define t-h
  (make-hash '((a . 4) (b . 3))))
(define t-l
  '((a 4) (b 3)))

(define-syntax (cmd-proc stx)
  (syntax-parse stx
    ((_cmd-proc cmd mach-id comp-id val)
     #'(hash-set! mach-id 'comp-id val))
    ((_cmd-proc cmd mach-id comp-id)
     #'(hash-ref mach-id 'comp-id))
    ((_cmd-proc cmd mach-id)
     0)))

(define-syntax (mk-ent stx)
  (syntax-parse stx
    ((_mk-ent id)
     (with-syntax ((new-id (format-id #'id "e-~a" (syntax-e #'id))))
       
       #'(begin

           ; data-structure for entity
           (define ent-hash (make-hash))
           (hash-set! ent-hash 'components #f)
           (hash-set! ent-hash 'controller #f)
           (hash-set! ent-hash 'attr #f)

           ; setters/getters/run
           (define-syntax (id stx)
             (syntax-parse stx
               ((_id (~literal dump))
                #'ent-hash)
               ((_id (~literal set) id val)
                #'(hash-set! ent-hash 'id val))
               ((_id (~literal get) id)
                #'(hash-ref ent-hash 'id))
               ((_id cmd)
                #'(let ((contr (hash-ref ent-hash 'controller))
                        (args cmd)
                        (comps (hash-ref ent-hash 'components)))
                    (contr args comps))))))))))

(define (graph-keeper cmd)
  "this is the graph-keeper")
(define (wallet cmd)
  "this is the wallet")

(mk-ent z)

(define m4
  '((lambda () (+ a b)) ((a 4) (b 3))))


(define-syntax (mk-env stx)
  (syntax-parse stx
    ((_mk-env id var-id)

     #'(define-syntax (id stx)
         (syntax-parse stx
           ((_id cmd)

            (with-syntax ((x-id (datum->syntax #'cmd (syntax-e #'var-id)))) ; <- donated lexical context needs to come from syntax given to THIS expander, not 'mk-env'
              
              #'(let ((x-id 456))
                  cmd))))))))



#;
(define-syntax (env-expand stx)
  (syntax-parse stx
    ((_env-expand env)     

     #'(begin
         
         (define ids (map (lambda (x)
                            (car x))
                          env))
         (define vals (map (lambda (x)
                             (second x))
                           env))
           
           ))))
       

(define-syntax (deep stx)
  (syntax-parse stx
    ((_deep name val)
     (displayln "we're in")
     #'(begin
         (define name val)
         (printf "here is val: ~a\n" name)))))

(define-syntax (blub stx)
  (syntax-parse stx
    ((_blub name val)
     (displayln "invoking 'blub'")
     ;(displayln (syntax? #'stx))
     ;(define code (deep #'name #'val))
     #'(deep name val))))


(define example

  ; closure
  (let ((display #f)
        (wait #f)
        (func-args (list 'x 'y))
        (attr-lst '(display wait)))

        
    ; top-level function
    (lambda (args)
      
      (define (cmd-handler lst-cmds)

        ;(displayln (list? lst-cmds))
       
        (define (handle-cmd cmd)

          ; maybe need unhygienic here...
          (define-syntax (handle-val stx)
            (printf "The syntax is ~a\n" stx)
            (syntax-parse stx
              ((_handle-val id)
               (printf "The id is ~a\n" #'id)
               (with-syntax ((ID (datum->syntax #'wait (syntax-e #'id))))
               
                 #'(printf "The value is: ~a\nAnd now: ~a" ID wait)))))
          
          ; need to catch the ACTUAL id higher up...?
          (define-syntax (handle-pair stx)
            (syntax-parse stx
              ((_handle-pair id val)
               #'(begin
                   (displayln id)
                   (if (member id attr-lst)
                     (set! id val)
                     'NO_SUCH_ATTR)))))
                   
          ; cmd dispatch  
          (if (pair? cmd)
              (displayln "PAIR");(handle-pair (first cmd) (second cmd))
              (begin
                ;(displayln "here")
                cmd
                (handle-val cmd))))

        ; loop through lst of commands and handle each (display results to std out)
        (for ((cmd lst-cmds))
          (displayln (handle-cmd cmd))))

      ; the embedded function, subject to wait and display requirements
      (define (core-func x y)
        (define (actual-func x y)
          (+ x y))
        (if wait
            (begin
              (printf "Sleeping for ~a seconds\n" wait)
              (sleep wait)
              (actual-func x y))
            (actual-func x y)))

      ; top-level dispatch -> looks for tagged list to identify cmds compared to standard procedure arguments
      (if (equal? (car args) 'cmd)
          (cmd-handler (cdr args))
          (core-func (car args) (second args))))))

#;
(define-syntax (mk-entity stx)

  (syntax-parse stx
    
    ((_mk-entity name core-func lo-attr)
     (with-syntax (((binding-pair ...) #'lo-attr))

       #'(begin

           (define (getter
           
           #'(define-syntax (name stx)
               
               (syntax-parse stx
                 
                 ((_name type data)
                  (displayln "get match")
                  (cond
                    ((eq? 'get (syntax-e #'type))
                   #'data)))
                 
                 ((_name cmd id val)
                  (displayln "set match")
                  (cond
                    ((eq? 'set (syntax-e #'type))
                     #'(set! id val)))))))))))))

#;
                  ((eq? 'call (car (syntax->list #'type)))
                   #'(core-func ,@(syntax->list #'data))
                  (else #'(error "Improper command")))

;(mk-entity tester (lambda (x y) (+ x y)) ((a 4) (b 3)))


; little toy for passing taking cmds to a macro and making them symbols for a subsequent function call ->
(define-syntax (passer stx)
  (syntax-parse stx
    ((_passer cmd ...)
     #'(t-func (list 'cmd ...)))))

(define (t-func cmd . other_cmds)
  (displayln "we're in")
  (let ((cmds (append cmd other_cmds)))
    (printf "commands are ~a\n" cmds)
    (for ((cmd cmds))
      (displayln cmd))))

; need to be able to look at components, including controller
; need access to ATTR (including meta-control like 'wait' and 'display')
; on meta-control, need a cmd that will cascade down to all below
; these entities/machines are really just virtual -> a component (function/data structure) inside the entity is just a REFERENCE, other machines can also reference same
; BUT REMEMBER, entities are made up of OTHER ENTITIES, a plain function is like a SIMPLE in this scheme
; a machine is components operating inside an environment. the environment contains

; maybe the entity is just a wrapper lambda that has an env and you pass it in top-level functions that are called in that env? ->
; that way all the getter's and setters and macros can be top level so you have access (see issues below)
; eh.. i don't think that works, OR, a function that takes a TRANSFORMER ID and invokes it inside?
; maybe modules are a help?
; can we have an entity-id macro take stx and then invoke a getter/setter macro in env?

; an entity needs to be FIRST-CLASS, so it has to be proc or data structure... I guess?
; how about a data structure that has components/attr/controller/etc which you hand to a macro for cmds ->
; (get a test-machine) -> expands into code to get a value from 'test-machine' data structure... are we just saving typing the "'" here...?

; !! let the interface/dispatch be expanded into w/ a macro (LOL 'dlambda' uses case and symbol comparison) and have it be set in the 'let' ->
; the, let's say, interface initiator can just be lambda that invokes the dispatch/interface as found in the let

; !! what if the closure is just data (quoted list)? we invoke it when we need it ->
(define t-closure
  '((a 4) (b 5)
          (comp1 (lambda (x y)
                         (+ x y)))
          (comp2 (lambda (x)
                   (+ a x)))))

(define t-syntax
  (datum->syntax #f t-closure))

(define-syntax (env stx)
  (syntax-parse stx
    ((_env id)
     (displayln (syntax-e #'id))
     (displayln (syntax->datum #'id))
     (with-syntax ((new-id (datum->syntax #'id 'a)))
     
       #'(let ((new-id 4)) ; <- have to actually use the SAME identifier we develop above, not just 'a'
           new-id)))))

(define-syntax (env2 stx)
  (syntax-parse stx
    ((_env bindings expr)
     
       #'(let bindings
           expr))))

(define-syntax (create stx)
  (syntax-parse stx
    ((_create bindings expr)
     (with-syntax ((new-id (datum->syntax #'id (syntax-e #'id))))
       #'(let ((new-id 4))
           expr)))))

(define-syntax (define-env stx)
  (syntax-parse stx
    ((_define-env id bindings)
     #'(define-syntax (id stx)
         (syntax-parse stx
           ((_id expr)
            (with-syntax ((new-id (datum->syntax #'expr 'v)))
              #'(define new-id
                  '(let bindings
                     expr)))))))))

(define-syntax (splice stx)
  (syntax-parse stx
    ((_splice expr)
     #'(let expr
         a))))

(define-syntax (tester stx)
  (syntax-parse stx
    ((_tester stuff)
     #'(let ((a 4))
         a))))

(define-syntax expand
  (lambda (stx)
    
    (syntax-parse stx
      ((_expand form)
       (printf "form: ~a\n" #'form)
       #'(let ((a 8))
           (begin0
             form
             (displayln "here we are")))))))

(define-syntax (take stx)
  (syntax-parse stx
    ((_take stuff)
     (define lst g)
     (define-values (ids vals)
       (for/lists (one two)
                  ((pair lst))
         (values
          (car pair)
          (cadr pair))))
     (define s-ids (map (lambda (x)
                          (datum->syntax #'stuff x))
                        ids))
     (define s-id-lst (datum->syntax #'stuff ids))
     (printf "lst: ~a\nids: ~a\ns-ids: ~a\nid-lst: ~a\n" lst ids s-ids s-id-lst)
     (with-syntax (((new-ids ...) s-ids)) 
       #'(let ((new-ids #f) ...)
           (displayln new-ids) ...))))) ; seems to work maybe...?

(define q
  '((a 9) (b 34)))

(define-syntax val
  (lambda (stx)
    #''((a 9) (b 34))))

(define (pass val)
  (expand val))



(define-for-syntax
  g '((a 4) (b 5)))

; give a macro a macro -> maybe use an IDENTIFIER MACRO
; (create bind-mac expr)
  

; could the macro define the closure? like have a macro take a list and define another macro that turns that list into a let and then takes syntax to inject into that env?
; problem with that is its not FIRST CLASS and how would you modify?


(define a 50)

          
                   
          
          

(define d-str
  `((a 4)
    (b 5)
    (contr (lambda (x y)
             (+ a (/ y b))))))

; could use gensym?
(define-syntax (proc-req stx)
  (displayln "here")
  (syntax-parse stx
    ((_proc-req env cmd (arg ...))
     
     ;(displayln (syntax-e #'env))
     
     #'(let (())))))

(define (proc-env env)
  (match env
    ((list (list id val) ...)
     (let ((ids id)
           (vals val))
       (printf "ids: ~a\nvals: ~a\n" ids vals)
       (proc-t id val)))))

(define-syntax (proc-t stx)
  (syntax-parse stx
    ((_proc-t ids  vals)
     #'(let ((lst `(,ids ,vals)))
         (displayln (car lst))
         (for ((pair lst))
           (displayln pair))))))


(define-syntax (proc-t2 stx)
  (syntax-parse stx
    ((_proc-t ids  vals)
     
     #'(let ((lst `(,ids ,vals)))
         (displayln (car lst))
         (for ((pair lst))
           (displayln pair))))))
     
         

(match d-str
  ((list (list id val) ...)
   (displayln (list id val))))



          


(define sample
  (let ((a 4) (b 5))
    (lambda (func args)
      (func args))))

(define-syntax (get stx)
  (syntax-parse stx
    ((_get id)
     (with-syntax ((new-id (datum->syntax #'id (syntax-e #'id))))
     #'new-id))))

(define (call-mac sym)
  (with-syntax ((mac-id (datum->syntax #f sym)))
    #'(mac-id a)))

(define-syntax (expand-to-x stx)
  (syntax-parse stx
    ((_expand-to-x something)
     (with-syntax ((id (datum->syntax #'something 'x))) ; <- will this be top-level if no context? YES, if we give it 'something's context, it works (see below)
       #'id))))                                         ; ie, we need to draw on SOMETHING from the calling site... some way around this? transformer w/ no stx arg?

(let ((x 4))
  (expand-to-x 'a))
  

(define t-mach
  
  (let ((attr (make-hash '((a 4) (b 5))))
        (components (make-hash '((comp1 #f) (comp2 #f))))
        (controller #f))

    (define (get [id #f])
      (if id
          (hash-ref attr id)
          (hash->list attr)))
    (define (set id val)
      (hash-set! attr id val))
    (define (get-contr)
      controller)
    (define (set-contr proc)
      (set! controller proc))
    (define (get-comp [id #f])
      (if id
          (hash-ref components id)
          (hash->list components)))
    (define (set-comp id val)
      (hash-set! components id val))

    ; interface -> what kinds of things to handle? does it handle programmatic cmds AND execution calls both? or should separate?
   
    (define-syntax (inter stx)
      (syntax-parse stx
        ((_inter cmd)
         #'(get))
        ((_inter cmd id)
         #'(get id))
        ((_inter cmd id val)
         #'(set! id val))))

    (lambda ()
      (begin
        (displayln `(,(hash->list attr) ,(hash->list components) ,controller))
        (inter get 'a))))) ; <- can use a function to call inner defined macro but have to use symbol...

; CONTROLLER will take "normal" args to machine and handle as defined
; but, controller only works in env where the components exist as defined...

; could have something like a CONTROL STACK which is where stuff like 'wait' and 'display' are called ->

; interface ->
#;
(lambda (cmd)
  (let ((result (controller cmd)))
    (begin
      (wait)
      (display cmd result)
      result)))


(define-syntax forever
  (syntax-rules ()
    ((forever body ...)
     (call/cc (lambda (abort)
                (let loop () body ... (loop)))))))

(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (forever (unless test (abort)) body ...))))


;(while #t (abort))

#; 
(forever (unless #t (abort)) (abort)) 

; ->
(call/cc (lambda (abort)
           (let loop ()
             (unless #t (abort))
             (abort)
             (loop))))


(struct t-struct (x y))

(define v
  (t-struct 1 2))

(define-syntax (return stx)
  (syntax-parse stx
    ((_get sym obj)
     (with-syntax ((selector-id (format-id #'sym "t-struct-~a" (syntax-e #'sym))))
       #'(selector-id obj)))))

; seems to work...
(define-syntax (mk-t-struct stx)
  (syntax-parse stx
    ((_mk-t-struct id init-vals ...)
     (with-syntax ((selector-id (format-id #'id "~a~a" (syntax-e #'id) (syntax-e #'id))))
       #'(begin
           (define id (t-struct init-vals ...))
           (define-syntax (selector-id stx)
             (syntax-parse stx
               ((_selector-id select-id)
                (with-syntax ((field-id (format-id #'id "t-struct-~a" (syntax-e #'select-id)))) 
                #'(field-id id))))))))))

(define-syntax (zy stx)
  (syntax-parse stx
    ((_zy ((name ...) (val ...)))
     #'(display
        '((name val) ...)))))
                              
      
    
        

      



          
  