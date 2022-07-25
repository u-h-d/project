#lang racket

(require sha)

(define (priv->pub priv)
  (sha256 (string->bytes/utf-8 (number->string priv))))

(provide priv->pub)