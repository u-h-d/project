#lang racket

; PROBLEM: can't write interchangeable functions that rely on bindings inside the environment ->
; COULD wrap these functions in a lambda that calls said core function with environment bindings ->
; (lambda (cmd) (core_func contr int actions attr etc))
; can even have these wrappers accept new core functions and install them ->

(define wrapper
  (let ((func #f))
    (lambda (cmd)
      (let ((tag (first cmd))
            (data (second cmd)))
        (if (symbol=? 'install tag)
            (set! func data)
            (if func
                (func data)
                (error "No function to call"))))))) ; <- something like this maybe?
        

; ENTITY ->
; (NAME CONTR INTERFACES  ACTIONS ATTR(STATE))

(define (mk-entity name contr int actions attr)
  
  ; entity-env
  (let ((running? #f)
        (name name)
        (controller contr)
        (interfaces (mk-int-table int))
        (actions (mk-action-table actions))
        (attr (mk-attr-table attr)))
   
    ; meta-contr -> starts machine or passes through commands
    (lambda (cmd)
      (cond
        (running?
         (controller cmd))
        ((symbol=? 'start cmd)
         (begin
           (set! running? #t)
           (controller cmd)))
        (else
         (error "Machine is not running"))))))


; _______________________________________________________________________________________________________________________________________________________________________________________
; ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; <COMPONENTS>

; controller ->
(define (controller cmd)
  (cond 
    ((component_request? cmd)
     (get-comp cmd))
    ((action_cmd? cmd)
     (perform cmd))
    ((install_cmd? cmd)
     (install cmd))
    (else
     (error "Invalid command"))))((symbol=? 'actions req) actions)


; get component value
(define (get-comp req attr actions int)
  
  ; are we asking for an entire set? -> 
  (cond
    ((symbol=? 'attr req) attr)
    ((symbol=? 'actions req) actions)
    ((symbol=? 'interfaces req) int)
    (else

     ; if not, get individual value
     (cond
       ((assoc req attr)
        (second (assoc req attr)))
       ((assoc req actions)
        (second (assoc req actions)))
       ((assoc req int)
        (second (assoc req int)))
       (else
        (error "Bad request"))))))
    

; initialize interfaces ->
(define (start interfaces)
  (for/list ((int interfaces))
    (int)))

; _______________________________________________________________________________________________________________________________________________________________________________________
; ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; <INTERFACES/ACTIONS> (for this entity)>

; network interfaces ->
(define (ntwk-int)
  (open-output-string))
(define (usr-int)
  (open-output-string))
(define (storage-int)
  (open-output-string))

(define int1
  (list ntwk-int usr-int storage-int))


  