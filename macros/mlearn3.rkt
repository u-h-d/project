#lang racket

(require (for-syntax syntax/parse racket/syntax))
(require racket/generator)

(define a 10)


; WAIT, so does a macro create a closure...?

(define (controller arg)
  (proc3 (proc2 (proc1 arg)))) ; <- can use macro call in lieu of actual proc call so we can draft controllers using certain names and then replace what that name stands for

(define (proc1 arg)
  (string-append "one " arg))
(define (proc2 arg)
  (string-append "two " arg))
(define (proc3 arg)
  (string-append "three " arg))

(define components
  `((proc1 ,proc1) (proc2 ,proc2) (proc3 ,proc3)))

#;
(define-syntax-rule (proc1 arg)
  ((second (assoc 'proc1 components)) arg))

(define-syntax (mk-ent stx)
  
  (syntax-parse stx
    ((_mk-ent id contr lst-comp lst-attr)

     (with-syntax ((dump-id (format-id #'id "~a-d" #'id))
                   (g-id (format-id #'id "~a-g" #'id))
                   (s-id (format-id #'id "~a-s" #'id)))
     
       #'(begin

           ; define fields of entity 
           (define entity
             (let ((val `((controller ,contr)
                          (components ,lst-comp)
                          (attr ,lst-attr))))
               (lambda ([set? #f])
                 (if set?
                     (set! val set?)
                     val))))
           
           ; for each component, define a macro that controller can use
           ; to make this generic would need a macro to define procs to change controller proc calls to be a macro that LOOKS UP proper proc
           (define-syntax-rule (proc1 arg)
             ((second (assoc 'proc1 (second (assoc 'components (entity))) arg)))) 
           (define-syntax-rule (proc2 arg)
             ((second (assoc 'proc2 (second (assoc 'components (entity))) arg))))
           (define-syntax-rule (proc3 arg)
             ((second (assoc 'proc3 (second (assoc 'components (entity))) arg))))
           
           ; define macros to set/get fields
           (define-syntax-rule (dump-id)
             (entity))

           (define-syntax-rule (g-id id)
             (second (assoc 'id (entity))))

           (define-syntax-rule (s-id val)
             (entity val))
           
           ; define macro for call to entity
             #;
           (define-syntax-rule (id arg)
             ((second (assoc 'controller (entity))) arg))

             )))))  

(define-syntax (copy-ent stx)
  (syntax-parse stx
    ((_copy-ent id n-id)
     (with-syntax ((d-id (format-id #'id "~a-d" #'id)))
       
       #'(mk-ent n-id (car (d-id)) (cadr (d-id)) '())))))

(mk-ent z
        controller
        components
        '())


  
             
#;
(mk-ent q
          (lambda (arg)
            (proc3 (proc2 (proc1 arg))))
          `((proc1 ,proc1) (proc2 ,proc2) (proc3 ,proc3))
          '())

(define-syntax (get-a stx)
  (syntax-parse stx
    ((_get-a id)
     #'(begin
         (let ((a 4)
               (x 12))
           
           (printf "~a\n~a\n~a\n" a x (+ a x)))
         (values
          a
          (+ 2 a))))))
    
  (define-syntax-rule (get-a2)
  a)

(define e-loop
  (infinite-generator
   (let loop ((seq '(1 2 3 4 5 6 7 8)))
     (yield (car seq))
     (loop (cdr seq)))))
    