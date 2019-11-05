#lang racket
(begin
  (define (list . xs) xs)
  (apply list (list 1 2 3)))

