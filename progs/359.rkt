#lang racket
(begin
  (define (f x) x)
  (apply f (cons 1 '())))
