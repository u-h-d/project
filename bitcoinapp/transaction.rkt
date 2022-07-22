#lang racket

; BITCOIN TRANSACTION and related

(require sha)
(require "utilities.rkt")


; transaction is utxo set -> utxo set (or a collection of tx ie block)

(struct single ((x #:mutable)))

; tx input is just a REFERENCE to a utxo
(struct transaction (locktime inputs outputs))
(struct input (reference unlock-script))
(struct output (amount lock-script))

; reference is SYMBOL (we will denote wallets w/ identifiers for now)
; a wallet will just be a NUM (balance)

; need SIGHASH flags...

; tx data is '(inputs (amt lock) (amt lock))

(define (mk-initial-utxo-set lo-wallets)
  (for/list ((wallet lo-wallets))
    (define amt (random 10000))
    (define lock-script (wallet 'pub))
    `(,lock-script ,amt)))

;(define (priv->pub priv)
 ; (sha256 (string->bytes/utf-8 (number->string priv))))

(define (mk-wallet)
  (let* ((private-key (random 1000000))
         (public-key (priv->pub private-key))
         (my-utxo #f))
    (lambda (req [data #f])
      (case req
        ((priv)  
         private-key)
        ((pub)
         public-key)
        ((update)
         (set! my-utxo (get-my-utxo public-key UTXO_SET)))
        ((dump)
         (printf "~a\n~a\n~a\n" private-key (bytes->list public-key) (list (bytes->list (caar my-utxo)) (cdar my-utxo))))
        (else (error "Invalid wallet command"))))))
        
    
(define (get-my-utxo address utxo-set)
  (filter
   (lambda (x)
     (equal? (first x) address))
   utxo-set))

(define z
  (mk-wallet))
(define j
  (mk-wallet))
(define b
  (mk-wallet))
(define g
  (mk-wallet))

(define wallet-set
  (list z j b g))

(define UTXO_SET
  (mk-initial-utxo-set (list z j b g)))


; a ref is a NUMBER, a witness is an unlock script (which is a list of script values ie symbols, strings or numbers)
(define (mk-input ref witness)
  `(,ref ,witness))
(define (mk-output amt lock-script)
  `(,amt ,lock-script))

(define (mk-raw-tx inputs outputs)
  ; presumably any program can make a tx
  ; not sure we need a message-passing function here... could just be a data-structure...
  ; need inputs and outputs ->
  `(,inputs ,outputs))


  #|
  (lambda (x)
    (cond
      ((symbol=? 'inputs) )
      ((symbol=? ) 'outputs)
      ((symbol=? ) 'data)
      ((symbol=? ) )

|#

(define (sign-tx tx)
  ; should this be exclusivley a wallet function?
  0)

    
(provide (all-defined-out))
          


