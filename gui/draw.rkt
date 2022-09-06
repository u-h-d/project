#lang racket

(require "pkg.rkt")

(require racket/gui)


; define a canvas
(define fr (new frame% (label "hello")))
(define c0 (new canvas% (parent fr)))
(define dc (send c0 get-dc))

; example draw-proc

; example draw-hook
; hook just needs to be tool to give draw-space updates
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
      (agents '())
      (input-hooks '())
      (output-hooks '())
      (e-name 'something))

  (lambda (cmd)
    "normal entity stuff"))
                     
; example draw controller ->
(define (draw-zh4)
  (define translated-space (translate fixed-space path-counter))
  `(,(draw-proc translated-space)
    ,(for/list ((ele elements))
       (ele fixed-space))))


#| 

   for every entity in a tree, install a new draw-space entity that knows about its assoc entity to draw,
   layout, fixed space its sub-elements assoc w/ same entity, and deeper draw-space entities assoc w/ deeper entities to draw
   might be a 'master' entity that manages these agents
   the agents take the fixed coords and draw based off them. 
   passes coords to subs which develop their own draw cmds.  parent collects and passes on to its parent
   each draw-entity can have path(s) which reset after a certain event and count down
   how to pass i/o hooks to sub-elements? who is going to find out about an i/o event?
   so draw agent embedded in entity, when installed, a corresponding hook is placed in entity linked to draw agent to provide
   as cmds (presumably?), i/o data when received.  the draw agents sub elements are themselves true entities
   question is whether we link to parent draw agent and pass to elements, or link to elements themselves...
   

|#

(send fr show #t)