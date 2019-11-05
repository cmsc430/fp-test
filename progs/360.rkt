#lang racket
(begin
  (define (f x y) y)
  (apply f (cons 1 (cons 2 '()))))

