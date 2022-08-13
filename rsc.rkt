#lang racket

(define (mr)

  (define (m-rec lst)
      
    (if (null? lst)
        'DONE
        
        (match lst
          ((list id subs ...)
           (begin
             (printf "Making entity: ~a\n" id)
             (let ((sub-e (get-subs subs)))
               (if (null? sub-e)
                   (displayln "No components to install...")
                   (for ((sub (get-subs subs)))
                     (printf "Installing component: ~a\n" sub))))
             (for ((sub subs))
               (m-rec sub)))))))

  (m-rec '(1
           (2 (5 (10)) (6))
           (3 (8))
           (4 (9 (11))))))
   

(define q
  '(1
    (2 (5) (6))
    (3 (8))
    (4 (9))))

   
(define (get-subs lst)
  (for/list ((entry lst))
    (car entry)))


(match q
  ((list id subs ...)
   (displayln id)
   (displayln (get-subs subs))))
