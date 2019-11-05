#lang racket
(begin
  (define (explode str)
    (explode/i str 0))
  (define (explode/i str i)
    (if (= (string-length str) i)
        '()
        (cons (string-ref str i)
              (explode/i str (add1 i)))))
  (explode "fred"))
