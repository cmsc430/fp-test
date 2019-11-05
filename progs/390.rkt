#lang racket
(begin
  (define (string->list s)
    (string->list/a s (string-length s) '()))
  
  (define (string->list/a s i cs)
    (cond [(zero? i) cs]
          [else (string->list/a s (sub1 i)
                                (cons (string-ref s (sub1 i)) cs))]))
  
  (string->list "abcde"))
