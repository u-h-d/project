#lang racket

#| TO DO:

learn more about serialization (and how #:prefab relates)


|#

(struct block
  (hash previous-hash transaction timestamp nonce)
  #:prefab)

; turn hashed elements from string to bytes, append them all, then feed to sha256, then turn to hex string
; why hex-string?
(define (calculate-block-hash previous-hash timestamp transaction nonce)
  (bytes->hex-string (sha256 (bytes-append
           (string->bytes/utf-8 previous-hash)
           (string->bytes/utf-8 (number->string timestamp))
           (string->bytes/utf-8 (~a (serialize transaction)))
           (string->bytes/utf-8 (number->string nonce))))))

; we check if a block is valid by seeing if its hash field equals a recalculation based on its other fields
(define (valid-block? bl)
  (equal? (block-hash bl)
          (calculate-block-hash (block-previous-hash bl)
                                (block-timestamp bl)
                                (block-transaction bl)
                                (block-nonce bl))))

; what is 'make-bytes'? -> returns MUTABLE byte string of LENGTH 'difficulty' where each byte is second arg
; again, why hex-string?
(define difficulty 2)
(define target (bytes->hex-string (make-bytes difficulty 32)))

; A block is mined if
(define (mined-block? hash)
  ; the hash matches the target, given the difficulty
  ; subbytes -> makes new byte-string based on positions provided by 2nd and 3rd arg (excludes last)
  ; here, simply selects, and then compares, the second byte
  ; so, we are trying to get the second byte to match -> higher difficulty would require MORE bytes to match (remember that sha256 produces a hash of a certain length every time)
  ; why not 1st byte?
  (equal? (subbytes (hex-string->bytes hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))

; Hashcash implementation
; we take the info for a potential new block and calculate the hash over and over, changing the nonce each time, until we hit our target
(define (make-and-mine-block
         previous-hash timestamp transaction nonce)
  (let ([hash (calculate-block-hash
               previous-hash timestamp transaction nonce)])
    (if (mined-block? hash)
        (block hash previous-hash transaction timestamp nonce)
        (make-and-mine-block
         previous-hash timestamp transaction (+ nonce 1)))))

; Wrapper around make-and-mine-block
; adds as field values current time and sets nonce to 1
(define (mine-block transaction previous-hash)
  (make-and-mine-block
   previous-hash (current-milliseconds) transaction 1))

(provide (struct-out block) mine-block valid-block? mined-block?)


(require (only-in file/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)

