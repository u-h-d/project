#lang racket

; global procedure that tracks time going forward
(define (m-operation time)
  
  ; how many loops?
  (define duration 10)
  
  ; the type of gem we seek in this operation ->
  (define gem #\z)
   
   ; the mine for our operation, a functional representation
   (define (mine)
     (integer->char (random 97 123)))
   
   ; a functional representation of a collection of miners
   (define (miners n)

     (define (mk-mmach mach-num)
       (list
        50
        100
        200
        20
        mach-num))

     (define (make-miners n)
       (cond
         ((= 0 n) '())
         (else
          (cons (mk-mmach n)
                (make-miners (- n 1))))))
     ; miner is a list of attributes
     ; defining the miner's attributs -> 
     (define (m-attr l tag)

       ; attr selectors ->
       (define (d-d)
         (car l))
       (define (p-c)
         (car (cdr l)))
       (define (chassis)
         (car (cdr (cdr l))))
       (define (storage)
         (car (cdr (cdr (cdr l)))))
       (define (number)
         (car l (cdr (cdr (cdr (cdr l))))))
         
       ; dispatch ->  
       (cond
         ((symbol=? 'drill-durability) (d-d l))
         ((symbol=? 'power-cell) (p-c l))
         ((symbol=? 'chassis) (chassis l))
         ((symbol=? 'storage) (storage l))
         ((symbol=? 'number) (number l))
         (else
          (displayln "Invalid machine attribute selector"))))

     ; mining is a miner => miner
     ; this miners mining function ->
     (define (mining-machine attributes)
       (cond
         ((= 0 (m-attr )
          (begin
            (displayln "Broken drill!\nFixing now!")))
         ;(return)))
         ((= gem-bag 20)
          (begin
            (displayln "Bag full, returning...")))
         ;(return))
         (else
          ; take from the mine and see if it's a desired gem
          (let ((next-yield (mine))
                (new-durability (- drill-durability 1)))
            (cond
              ((char=? #\z next-yield)
               (begin
                 (displayln "Gem found!")
                 (mining-machine
                  new-durability
                  (+ 1 gem-bag))))
              (else
               (begin
                 (displayln "Mining...")
                 (mining-machine
                  new-durability
                  gem-bag)))))))) 
     ; return a function that takes initiating arguments to launch a mining-machine
     (lambda (d-d g-b)
       (mining-machine d-d g-b)))
   ; continue looping until we reach our duration...
   (if (< time duration)
       ; initiate mining machine ->
       (begin
         ((miners) 50 0)
         (m-operation (+ 1 time)))
       (display "THE SIMULATION IS OVER")))

; need a way to simulate movement between places...
(define (return)
  0)
         