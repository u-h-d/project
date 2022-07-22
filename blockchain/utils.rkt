#lang racket

; This procedure returns true if the predicate satisfies all members of the list
(define (true-for-all? pred list)
  (cond
    [(empty? list) #t]
    [(pred (car list)) (true-for-all? pred (cdr list))]
    [else #f]))

; Export a struct to a file
; open a file (if it exists) w/ tag to replace its contents
; serialize, then write, a struct to said file
(define (struct->file object file)
  (let ([out (open-output-file file #:exists 'replace)])
    (write (serialize object) out)
    (close-output-port out)))

; Import struct contents from a file
; open a file, read it into memory, close file, then deserialize result
(define (file->struct file)
  (letrec ([in (open-input-file file)]
           [result (read in)])
    (close-input-port in)
    (deserialize result)))

; 
(define (file->contract file)
  (with-handlers ([exn:fail? (lambda (exn) '())])
    (read (open-input-file file))))

(provide true-for-all? struct->file file->struct file->contract)

(require racket/serialize)
