#lang racket
(begin
  (define (f x y . zs) x)
  (f 1))
