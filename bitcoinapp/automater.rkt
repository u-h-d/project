#lang racket

(require "transaction.rkt")

(define AMT_RANGE '(1 100))

; loop to randomly create transactions between nodes ->
(define (network-loop)
  (sleep (random 1 5))
  (let* ((sender-utxo (list-ref UTXO_SET (random 0 (- (length UTXO_SET) 1))))
         (sender-address (car sender-utxo))
         (recipient-address (car (list-ref (remove sender-utxo UTXO_SET) (random 0 (- (length UTXO_SET) 1)))))
         (amt (random (car AMT_RANGE) (second AMT_RANGE))))
    (printf "Transaction from ~a to ~a in the amount of ~a\n" sender-address recipient-address amt)
    (network-loop)))

(define (network-loop2)
  (sleep (random 1 5))
  (let* ((sender (list-ref wallet-set (random 0 (- (length wallet-set) 1))))
         (recipient (list-ref (remove sender wallet-set) (random 0 (- (length wallet-set) 1))))
         (amt (random (car AMT_RANGE) (second AMT_RANGE)))
         



