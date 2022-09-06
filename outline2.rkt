#lang racket

(require (for-syntax syntax/parse))

(module above racket
  
  (require (for-syntax syntax/parse racket/syntax))

  (provide (all-defined-out) ) ; doing this provides any ID's made w/ macros... is there a better way?

  ; define setters/getters -> simply translates to entity proc call
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


  
  (define-syntax (with-env stx)
    (syntax-parse stx
      ((_mk-run (bindings ...) body)

       #:with binding-syntax (map (lambda (binding)
                                    
                                    (let ((id (car binding))
                                          (val (cadr binding)))
                                      `(,(format-id #'comp-id "~a" (syntax-e id))
                                        ,val)))
                                    (syntax->list #'(bindings ...)))
                                    

       #'(let binding-syntax
             body))))
  
  
  ; helper functions
  (define in-debug
    (lambda (arg)
      (printf "Input is ~a\n" arg)))
  (define out-debug
    (lambda (arg)
      (printf "Output is ~a\n" arg)))
  (define wait
    (lambda (arg)
      (sleep 1)))
  
  ; run-hook procs
  ; we assume each hook acts independently..
  (define (run-hooks lst-hooks val)
    (if (null? lst-hooks)
        (void)
        (begin
          (for ((hook lst-hooks))
            (hook val)))))
  
  
  (define (e-print ht)
    ; core recursive
    (define (print-r ht layer)
      ; take list of keys and initiate for each one
      (define (print-init-all lst-keys layer)
        (for/fold ((acc-str ""))
                  ((key lst-keys))
          (let ((proc (hash-ref ht key)))
            (string-append acc-str
                           (print-r (proc '(get-all))
                                    layer)))))
      
      (let* ((keys (id-filter  ht))
             (name (hash-ref ht 'e-name))
             (name-str (format "~a~a\n" (make-string layer #\ ) name)))
        
        (if (null? keys)
            name-str
              (format "~a~a"
                      name-str
                      (print-init-all keys (+ 1 layer))))))
            
    (print-r ht 0))

  
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
             
             ; define our entity as a closure
             ; might want a way to dump expr that makes controller...
             (define name
               
               (let* ((components (make-hash `((e-name . name)
                                               (controller .
                                                           ,(lambda (input)
                                                              'controller))
                                               (controller-datum . #f)
                                               (input-hooks . (,in-debug ,wait))
                                               (output-hooks . (,out-debug ,wait))))))
                                             

                 ; would it be possible to have simple expressions in hook list that are eval'd in this closure?
                 
                 (hash-set! components 'ids vals) ...


                 (define (call-w/env)
                   (define keys
                     (id-filter (hash-keys components)))
                   (define bindings
                     (map (lambda (key)
                            `(,key ,(hash-ref components key)))
                          keys))
                   (define body (hash-ref components 'controller))
                   (with-env bindings body))
                 
                 ; core dispatch 
                 (lambda (cmd)
                   (case (car cmd)
                     ; set command
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
                          (hash-set! components (second cmd) (third cmd)))))
                     ; get command
                     ((get) (hash-ref components (second cmd)))
                     ((get-all) components)
                     ((print) (e-print components))
                     
                     ; main function
                     ; might want hooks to know about other things like 'name' etc...
                     ((run)
                      (begin
                        (printf "Starting ~a ...\n" 'name)
                        (run-hooks (hash-ref components 'input-hooks) cmd) ; this might be where translation layer comes in
                        (define output
                          ((hash-ref components 'controller) cmd)) ; want to run on naked 'cmd' bc that is the actual format expecte by program
                   
                        (run-hooks (hash-ref components 'output-hooks) output)
                        output))))))))))))

(require 'above)

(define-syntax (mk-all stx)

  (syntax-parse stx
    ((_mk-all name ((comp-id sub-comps) ...))
     #'(begin
         (mk-entity name ((comp-id #f) ...))
         (mk-all comp-id sub-comps) ...
         (name `(set comp-id ,comp-id)) ...))
     
    ((_mk-all name ())
     #'(mk-entity name ()))))

(mk-all zh4
        ((subc1 ((dd1 ())
                 (dd2 ())))
         
         (subc2 ((dd3 ())
                 (dd4 ((dd5 ())))))))


(define-syntax (defcontr stx)
  (syntax-parse stx
    ((_defcontr name body)
     #'(name '(set controller body)))))