#lang racket

(require (for-syntax syntax/parse racket/syntax))

(struct hook (name tags proc) #:mutable)

(provide (all-defined-out))


(define (run-hooks hooks input)
  (for ((hook hooks))
    (let ((proc (hook-proc hook)))
      (proc input))))

(define (mk-e (name #f))

  (let ((components (make-hash))
        (agents (make-hash))
        (controller (lambda (input)
                      input))
        (input-hooks '())
        (output-hooks '())
        (e-name name))

    (lambda (cmd)

      (case (car cmd)

        ((set)
         (begin
           (displayln 'new-component)
           (hash-set! components (cadr cmd) (caddr cmd))))
        ((get)
         (hash-ref components (cadr cmd)))
        ((get-all) components)
        ((set-controller)
         (set! controller (cadr cmd)))
        ((set-agent)
         (hash-set! agents (cadr cmd (caddr cmd))))
        ((get-controller) controller)
        ((get-ihooks) input-hooks)
        ((get-ohooks) output-hooks)
        ((get-name) e-name)
        ((set-input-hook)
         (set! input-hooks (cons (cadr cmd) input-hooks)))
        ((set-output-hook)
          (set! output-hooks (cons (cadr cmd) output-hooks)))
        
        ((run)
         (begin
           (define arg (cadr cmd))
           (run-hooks input-hooks arg)
           (define result (controller arg))
           (run-hooks output-hooks result)
           result))

        (else 'ERRO-NO-MATCH)))))


(define q
  '(zh4
    (subc1 (dd1 (sdd6)) (dd2))
    (subc2 (dd3))
    (subc3 (dd4 (sdd7)))))


(define z
  (mk-e))


;mk new and pass w/ subs -> for each sub, install result of recurring w/ mk new and ITS subs -> when no subs, return

(define (ma lst name)

  (define (ma-rec parent lst)
    
    (for ((sub lst))
      (begin
        (parent `(set ,(car sub) ,(ma-rec (mk-e (car sub)) (cdr sub))))))

    parent)
    
  (ma-rec (mk-e name) (cdr lst)))

(define (print-e e)

  (define (p-rec parent level)
    (define padding (make-string level #\ ))
    (for/fold ((str ""))
              ((comp-key (hash-keys (parent `(get-all)))))
      (string-append str padding (format "~a <input-hooks> ~a  <output-hooks> ~a\n~a"
                                         comp-key
                                         (parent '(get-ihooks))
                                         (parent '(get-ohooks))
                                         (p-rec (parent `(get ,comp-key)) (+ 1 level))))))
    

  (printf "TOP\n~a" (p-rec e 0)))

(define (install-all e hook-template)

  (define init-proc ((caddr hook-template) e))

  (define init-hook (hook (car hook-template)
                          (cadr hook-template)
                          init-proc))

  (define comp-keys (hash-keys (e '(get-all))))
  (displayln comp-keys)
  
  (cond
    ((null? comp-keys)
     (begin
       (e `(set-input-hook ,init-hook))
       (e `(set-output-hook ,init-hook))))
    (else
     (begin
       (e `(set-input-hook ,init-hook))
       (e `(set-output-hook ,init-hook))
       (for ((key comp-keys))
         (let ((nxt-e (e `(get ,key))))
           (install-all
            nxt-e hook-template)))))))

; contr data will be
; this works and is simpler than what is in 'outline3'... hmmm
(define-syntax (install-contr stx)
  (syntax-parse stx
    ((_install-contr entity (comp ...) body)

     #:with (comp-id ...) (map (lambda (datum)
                                        (format-id #'entity "~a" (syntax-e datum)))
                                      (syntax->list #'(comp ...)))

       #'(let ((comp-id (entity '(get comp-id))) ...)
           body))))

(define (dummy-hook cmd)
  'here)

(define zh4
  (ma q 'zh4))


;; what should be the format of hooks? ->
;; may want generic install-hook function that can look at tags and put in right place
;; probably  at least need a name so we can update/remove
;; hook could be a struct...
;; hook seed can also be a lambda that takes the entity and builds and returns the actual hook...

(define basic
  `(
   'basic
   ()
   ,(lambda (entity)
      (lambda (arg)
        (let ((name (entity '(get-name))))
          (printf "~a's basic hook is passing through: ~a\n" name arg))))))
  
                           
; !! REMBMER the idea about a meta-entity that maintains a list of packages and the types assoc therewith
; for auto updating and smart installing...

; install-hooks can be told in/out/both, single, all or so many layers
(define (install-hook e hook w?)

  ; init hook proc by passing entity and setting proc to result
  (set-hook-proc!
   hook ((hook-proc hook) e))
  
  (case w?
    ((in)
     (e `(set-input-hook ,hook)))
    ((out)
     (e `(set-output-hook ,hook)))
    ((both)
     (e `(set-input-hook ,hook))
     (e `(set-output-hook ,hook)))))

; might want a 'crawl-all' proc ->

(define (crawl-e e proc)
  (displayln e)
  (begin
    (proc e)
    (let ((comps (hash-keys (e '(get-all)))))
      (if (null? comps)
          'DONE
          (for ((comp comps))
            (let ((nxt-e (e `(get ,comp))))
              (displayln nxt-e)
              (crawl-e nxt-e proc)))))))

(define (tproc e)
  (displayln (e '(get-name))))

(define (install-plugin entity plug-in)
  (crawl-e entity plug-in))

; a plug in is a function that takes an entity and installs itself
; 'draw' could either be a "manager" or "agent" or an encompassing entity...

(define (draw-plug-in entity)

  (define (draw-agent)
    'draw-agent)
  ; need canvas
  (begin
    (entity '(set-agent))))

;; !! maybe controller is a thunk ->
;; (lambda ()
;;    (lambda (comp1 comp2)
;;         ...
;;     )
;;  ((hash-ref 'comp1 components) (hash-ref 'comp2 components)))
