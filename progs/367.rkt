#lang racket
(begin
  (define (sum . xs)
    (if (empty? xs)
        0
        (+ (car xs)
           (apply sum (cdr xs)))))
  (sum 1 2 3 4))
