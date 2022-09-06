#lang racket

(require "pkg.rkt" (for-syntax "pkg.rkt"))

(require racket/gui)


; define a canvas
(define fr (new frame% (label "hello")))
(define c0 (new canvas% (parent fr)))
(define dc (send c0 get-dc))

; example draw-proc

; example draw-hook
; hook just needs to be tool to give draw-space updates
; this version inits a relay hook w/ a given draw space proc
(lambda (draw-space)
  
  (lambda (data)
    (draw-space `(new-input ,data))))

(struct path ((counter #:mutable)
              sub-paths))

; example element
; sub-path just needs to be a step value for x and y
; path is list of sub-paths
; path-counter is a list of how many steps to take
; need way to step through multiple sub-paths AND
; maintaining where last sub-path ended...
#;(let ((space #f)
      (path '((2 0) (0 3)))
      (path-counter 0))
  
  (lambda (cmd)

    (case (first cmd)
      ((start)
       (begin
         (set! path-active?
               `((init-path (car path))
                 (rest path)))
         (get-trans path-active?)))
      ((nxt)
       (if (null? path-active?)
           (get-trans path-active?)
           (begin0
               (get-trans path-active?)
             
             
; example draw-space
; gonna need to pull info from entity, and
; get updates from hooks...
; this will live as meta-component in entity...
; when installed will be given entity so THEN we
; can link the two
(define (mk-draw-space entity)

  (let ((space 'relative-space)
        (fixed-space 'false-until-fixed)
        (entity entity) 
        (draw-proc #f)
        ; elements are things to draw but can be moved
        ; maybe EVERYTHING should be an element -> let it be a kind of data structure (maybe a closure)
        (elements '(input output))
        (paths '((input #f) (output #f)))
    
    (lambda (cmd)
      
      (case (first cmd)
        ((draw) (draw-proc entity))
        ((new-input) ))))))

(define (mk-draw-mgr entity)

  (define cc
    (let ((draw-cmds '()))
      (lambda (new-cmd)
        (set! draw-cmds (cons new-cmd draw-cmds)))))

  (let ((dc dc)
        (cmd-collector cc))
    
                          
        ))))))))


  ; question is whether something like a draw manager is 1) an encompassing entity and 2) whether it is an entity at all...
  ;

#;(define (draw-proc space entity elements dc)

  (let ((x0 (car space))
        (y0 (cadr space))
        (w (third space))
        (h (fourth space))
        (name (symbol->string (entity '(get-name)))))
        
    ; generate a list of draw-cmds (one for each draw-element of this draw-space)-> do they need to be thunk?
    (for/list ((element elements)) ; element should have base offset and then any active translations 
      (lambda ()
        (get-element-draw-cmd element x0 y0 w h)))))

; an element is:


; current thinking -> 'draw-mgr' needs to be an actual entity ->
; its components will be, among other things, the entity we want to draw ->
; maybe a recursive situation where we install draw-entities into structure

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
                     
; example draw controller ->
; idea is to call 'draw-proc' w/ whatever fixed-space we have, appropriately translated, put in list with whatever result we get
; from our elements
#;(define (draw-zh4)
    (define translated-space (translate fixed-space path-counter))
    `(,(draw-proc translated-space)
      ,(for/list ((ele elements))
         (ele fixed-space))))

(define draw-mgr (mk-e 'draw-mgr))
(define comp-list '(draw-proc assoc-entity layout fixed-space elements subs path path-counter))
(for ((comp comp-list))
  (draw-mgr `(set ,comp #f)))

; !! PROBABLY really need to make standard be that we strip off meta-cmd stuff when we pass into 'run' part of entity

#;(define d-contr
  (install-contr
   draw-mgr
   (draw-proc assoc-entity fixed-space elements subs path path-counter)

   (lambda (cmd) ; cmd will be a fixed-space coming from above...

     (define (draw-em)
       ; draw to translated (if any) fixed-space
       (define new-fixed-space (find-in-super layout (cadr cmd)))
       (define translated-space (translate new-fixed-space path path-counter))
       (define parent-draw-thunk (draw-proc translated-space))
       ; collect list of draw-thunks for elements
       (define element-thunks
         (for/list ((element elements))
           ; call each element, which is a draw-proc thunk maker,  in a closure, with translated fixed-space
           (element translated-space)))
       (define sub-thunks
         (for/list ((sub subs))
           (sub '(run translated-space))))
       `(,parent-draw-thunk ,element-thunks ,sub-thunks))


     (define (hook-alert)
       ; do whatever we need to with hook info..
       (define hook-data (cadr cmd))
       'hello)

     ; core dispatch
     (case (car cmd)
       ((hook-data) (hook-alert))
       ((draw) (draw-em))))))

;; !! NEXT UP, DRAFT A META-MGR THAT CAN INSTALL AND MANAGE DRAW-AGENTS

; example entity
(define draw-mmgr
  (let ((draw-entity zh4)
        (spawn '()) ; remember distinction between a MRG children and its 'agents' field (as all entities have..)
        (agents '())
        (mk/install-agent (lambda (entity) ; could the function itself house the type-list?
                    'mk-agent))
        (controller (lambda (cmd)
                      'controller))
        (i-hooks '())
        (o-hooks '()))

    (lambda (cmd)
      "generic entity stuff"
      ; example controller
      (define (example-contr cmd)
        (case (car cmd)
          ((mutate-event) (mk/install-agent (second cmd))) ; second will presumably be the entity/sub-entity which is new and needs insta 
          ((run-install) (mk/install-agent (second cmd))) ; looks like can probably combine these...
          ; might need to maintain data in entity that says what flags/options a given manager should install with for that entity
          ; so when we auto re-install we follow previous standards

          )))))

; !! we want our agents in an entity to have a list of functions that run depending on the type of event ->
; !! mutation event or standard run event or something else -> could have a MUTATE HOOK so don't have to maintain separate list
; !! also, we can maintain a list in agent-mgr that can have separate agent constructors for different entity-types ->
; !! on installation, MGR extracts proper constructor for a given TYPE of enttiy to install in -> how does it work though
;    when we have a custom agent we placed interactively that deviates from the script?
        
; example entity  w/ draw-agent ->
(define ex-ent
  (let ((subc1 #f)
        (subc2 #f)
        (subc3 #f)
        (agents `((draw-agent . ,(lambda (arg)
                                   'do-something))))
        (i-hooks `(,draw-agent-ihook))
        (o-hooks `(,draw-agent-ohook))
        (mutate-hooks `(,draw-agent-mutate-hook)) ; <-this is an option too...
        (mutate-event-list `((,draw-agent))))

    (lambda (cmd)
      (case (car cmd)
        ((set)
         "do the setting"
         (run-hooks mutate-hooks)) ; <- this would alert either the draw-agent OR draw-mgr directly... how would that look?
        ))))
        

(define agent-mgr ; could a letrec work here for being able to refer to a function we need a spawned hook to call?
  (letrec ((e-name "name") ; or let*
           (controller (lambda (cmd)
                         'does-something))
           (agent-list '())
           (mk-spawn (lambda (entity)
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
                         (cons `(,name ,agent) agent-list))))) ; stick our newly spawned agent in list...
    "do MGR stuff"))


#| 

   for every entity in a tree, install a new draw-space entity that knows about its assoc entity to draw,
   layout, fixed space its sub-elements assoc w/ same entity, and deeper draw-space entities assoc w/ deeper entities to draw
   might be a 'master' entity that manages these agents
   the agents take the fixed coords and draw based off them. 
   passes coords to subs which develop their own draw cmds.  parent collects and passes on to its parent
   each draw-entity can have path(s) which reset after a certain event and count down\
   how to pass i/o hooks to sub-elements? who is going to find out about an i/o event?
   so draw agent embedded in entity, when installed, a corresponding hook is placed in entity linked to draw agent to provide
   as cmds (presumably?), i/o data when received.  the draw agents sub elements are themselves true entities
   question is whether we link to parent draw agent and pass to elements, or link to elements themselves...

 RESTART THINKING:

a draw agent creates static draw-cmd until some event
one event would be a change in space parameter
a reason to make every element a true entity is for modularity (ie, subbing draw cmds)
could use a simpler structure...

|#

; define stuff for imported zh4 entity
(install-all zh4 basic)

(define n-contr
  (install-contr
   zh4
   (subc1 subc2 subc3)
   (lambda (cmd)
     (define r1 (subc1 `(run ,cmd)))
     (define r2 (subc2 `(run ,r1)))
     (subc3 `(run ,r2)))))

(zh4 `(set-controller ,n-contr))








; (send fr show #t)
