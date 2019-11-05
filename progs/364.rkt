#lang racket
(begin
  (define (f x y . zs) zs)
  (f 1 2 3))
