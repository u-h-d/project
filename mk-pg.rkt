;#lang racket
#lang web-server/insta



(define (start req)
  (response/xexpr
   (pg-mk)))




; *REMEMBER, div's can be NAMED -> (div ((id "name")))
(define (pg-mk)
  (define (ele)
    `((div (,(name) ,(style))
          ,(content))))

  ; main driver
  `(html
    (h1 "Testing")
    ,@(ele)))

(define (name)
  '(id "Zach"))

(define (style)
  '(style "text-align:center"))

(define (content)
  '(p "Here is some stuff"))

(define (t-mk)
  `(div (,(name) ,(style))
        ,(content)))
  