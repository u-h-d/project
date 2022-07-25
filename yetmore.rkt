#lang racket

; !! consider that what we should strive for would be a generic entity with a small set of operations to build/modify said entity ->
; !! may need a way to tag a certain TYPE of attr so if we make a cascading move on a nested structure containing that type, we can find
; -> the point of the entity/closure model is composability, iter-ability and hook-ability
(define (mk-entity (init-vals #f))

  (let ((attr (make-hash)))

    ; proc needs to be able to take in updated values for our attributes and place them in the table
    (lambda (cmd)
      (match cmd
        ((list tag val)
         (displayln tag)
         (displayln val))))))

; potential cmds ->

; install: add a key/val pair to entity hash
; install-set: add a named-sub-hash to entity hash
; install-all: add one of the above to an entity and all its components of a certain type
; dump: return an entities hash of attributes
; anytime we add an attr, we need to have setters/getters for said attr
  ; if we're looking for a nested key, standard might be to have a concat symbol -> (find 'comp-node') to find an entity named 'node' that is in the set 'comp'

; !! may have to have a standard where an entity needs component called 'controller' or something, so we know where to put our hooks...
; is the controller a black-box? or can we hook inside? -> sub-entities we can obviously work with
; maybe have a way to notate a function and use a macro to create hook locations
; is a controller just another entity w/ attr?

; DRAW-MGR ->

#|

holds an entity tree and a drawing context
assigns tree fixed spaces for them to use their drawing function to generate draw-requests
periodically collects drawing requests and performs them

an entity will draw its input and output, handing its output to the appropriate caller
a superior entity may wish to have represent the collection/transformation of multiple stages of value composition

|#