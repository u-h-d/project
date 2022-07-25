#lang racket

(define (something x)
  (* x 2))

(define (mk-ent (init-vals #f))

  (let ((attributes (make-hash)))

    (lambda (cmd)
      "hello")))

(define controller-example

  (let ((func1 +)
        (func2 *)
        (func3 -))

    (lambda (x)

      (func2
       (func2
        x
        (func3 x 3))
       x)))) 


;; if we have an entity ->
(let ((stuff '())
      (more-stuff '()))

  (lambda (cmd)

    ; initial parsing... (may have cmd tags and such we don't want to include as 'input'
    #; (init-parse cmd)

    ; then, bind what we decide to consider the entities INPUT ->
    (define input '())

    ; call whatever needed w/ 'input' (drawing-mgr, logger, etc)
    #;(process-input input)

    ; do the actual work the entity is made for, and bind it ->
    #;(define output (controller input))
      
    ; the controller is just another entity, but typically we'd expect it to be FLAT ->
    ; ie, ITS entities won't be nested
    ; controller wouldn't have a separate draw-space from entity (in a sense it IS entity)
    ; BUT, we want to be able to to have hooks in its process
    
    ; now, process output data, then return for the rest of the program to use
    #;(process-output ouput)
    #;output
    ))

