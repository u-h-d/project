#lang racket

; generic constructor for making 'entities' 


; **NEED SOME WAY TO PULL FROM ENVIRONMENT THOSE THINGS THAT MIGHT BE NEED BY UPDATING PROCEDURES
; **TO DO THEIR WORK...

; **LIKE, AN ENTITY COMES INTO AN ENV AND INTERFACES WITH IT TO DISCOVER RELEVANT INFO...
; entity could alert to attr of env and entity could be updated to accomodate

; **WHAT ABOUT ATTR THAT ARE JUST A FUNCTION THAT ENCAPSULATES THE VALUE?


; an entity is a list of: control and list of: attr
; OR, an entity is a function that when called gives you the next entity...
; a control is a function that dictates entity behaviour
; an attr is a value and a function that updates that value

; entity selectors...
(define (controller e)
  (car e))
(define (attr e)
  (car (cdr e)))

; atrr selectors...
(define (value attr)
  (car (cdr (car (cdr attr)))))
(define (update-proc attr)
  (car (car (cdr attr))))


(define (retrieve-attr tag lst)
  (let ((val? (assoc tag lst)))
    (if val?
        (car (cdr val?))
        #f)))

#|  
(define (entity func list-of-attr)
  (lambda (tag)
    (cond
      ((symbol=? 'nxt tag)
       (func list-of-attr)
       (entity func (update-attr list-of-attr)))
      (else (retrieve-attr tag)))))
|#

(define (update-attr attr)
  (printf "attr is ~a\n" attr)
  (list (car attr)
        (list (update-proc attr)
              ((update-proc attr) (value attr)))))

(define (update-all-attr loa)
  (cond
    ((null? loa) '())
    (else
     ; pop first pair, name value and function for updating, get new val by calling function on val
     ; then make new attr pair and cons it onto the rest of the list that will be made by recurring and updating the rest of the attr pairs
     ; might strive for update functions to pull from env data in update process
     ; also, how can we have an update mechanism where everything is intertwined and influences each other?
     (cons (update-attr (car loa))
           (update-all-attr (cdr loa))))))

(define (new-drill)
  (list 'drill
        (list (lambda (current-durability)
                (- current-durability 1))
              100)))

(define (new-storage)
  (list 'storage
        (list (lambda (current-storage)
                (- current-storage 5))
              50)))
    

(define (m-func attr)
  ; process everything based on attr (location, capacity, etc), then mk new version of self ->
  (mk-entity m-func (update-all-attr attr)))

(define (mk-entity controller loa)
  ;(let ((machine (list controller loa)))
    (lambda (tag [n-contr '()])
      (cond
        ((symbol=? 'attr tag)
         loa)
        ((symbol=? 'controller tag)
         controller)
        ((symbol=? 'nxt tag)
         (controller loa))
        ((symbol=? 'new-contr tag)
         (mk-entity n-contr loa))
        (else
         (let ((retrieve? (retrieve-attr tag loa)))
           (if retrieve?
               retrieve?
               'INVALID-COMMAND))))))

; ok, but how does the controller dictate behaviour outside of explicit commands? would it just be 'nxt?
; might have, on creation, populate data structure (list?) that can be returned to tell you all the attr
; how can we also have ability to insert new attr?

(define m-attr
  (list
   (new-drill) 
   (new-storage)))

(define m1
  (mk-entity m-func m-attr))
       

(define (run-updates e n)
  (cond
    ((= 0 n) e)
    (else
     (let ((new-e (e 'nxt)))
       (run-updates new-e (- n 1))))))

; HOW TO DO FUNCTION ONLY VERSION...?
(define spd
  (let* ((speed 0)
         (update
          (lambda ()
                 (set! speed (+ 1 speed)))))
  (lambda (tag)
    (cond
      ((symbol=? tag 'val)
       speed)
      ((symbol=? tag 'update)
       (update))
      (else
       'ERROR)))))
  
  
