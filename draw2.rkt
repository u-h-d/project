#lang racket

(require "pkg.rkt" (for-syntax "pkg.rkt"))

(require racket/gui)


; define a canvas
(define fr (new frame% (label "hello")))
(define c0 (new canvas% (parent fr)))
(define dc (send c0 get-dc))

; current thinking -> 'draw-mgr' needs to be an actual entity ->
; its components will be, among other things, the entity we want to draw ->
; maybe a recursive situation where we install draw-entities into structure


(define tda
  
  (let ((components (make-hash
                     `((e-to-draw . '())
                       (draw-proc . #f)
                       (layout . ())
                       (fixed-space . #f)
                       (elements . (name input output))
                       (sub-draw . (hash-keys (e-to-draw '(get-all))))
                       (path . ((2 10) (-2 10)))
                       (path-counter 0))))
        (controller 'some-controller)
        (agents '())
        (input-hooks '())
        (output-hooks '())
        (e-name 'something))
    
    (lambda (cmd)
      
      "normal entity stuff"))
                     
; example entity  w/ draw-agent ->
#;(define ex-ent
  (let ((subc1 #f)
        (subc2 #f)
        (subc3 #f)
        (agents `((draw-agent . ,(lambda (arg)
                                   'do-something))))
        (i-hooks `(,draw-agent-ihook))
        (o-hooks `(,draw-agent-ohook))
        (mutate-hooks `(,draw-agent-mutate-hook)) ; <-this is an option too...
        (mutate-event-list `((,draw-agent)))) ; is it this OR above...?

    (lambda (cmd)
      (case (car cmd)
        ((set)
         "do the setting"
         (run-hooks mutate-hooks)) ; <- this would alert either the given hook OR draw-agent OR draw-mgr directly... how would that look?
        ))))

  ; agent should do its work and collect the work of its subs
  ; events should cascade down (change in space-size) so everything can update.

(define agent-mgr ; could a letrec work here for being able to refer to a function we need a spawned hook to call?
  ; not consistent w/ entity approach...
  (let ((e-name "name") ; or let*
        (controller (lambda (cmd)
                      'does-something))
        (agent-list '())
        (components (make-hash
                     ((mk-spawn . (lambda (entity)
                                    (define name (mk-spawn-id e-name)) ; what was this for? making a unique symbol that depends on entity we install into?
                                    (begin
                                      ; define the agent -> should this be an entity?
                                      (define agent
                                        (lambda (cmd)
                                          (define e entity) 
                                          ("do agent stuff")))
                                      (entity `(set-agent ,name ,agent)) ; place newly defined agent in entity
                                         (entity `(set-mutate-hook ,(mk-id name) (lambda (data)
                                                                                   (controller data)))) ; OR could put in 'agent'
                                         (cons `(,name ,agent) agent-list))))) ; stick our newly spawned agent in list of agents in service
                     "do MGR stuff")))))

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
  (let ((name "enclosing-entity-name/contr")
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
  (lambda (entity)
    
        
  ; thinking about draw-agent controller ->
