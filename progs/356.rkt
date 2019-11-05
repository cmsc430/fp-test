#lang racket
(begin
  (define (f x) (+ (g x) 2))
  (define (g x) (+ (add1 #f) 2))
  (f 3))
