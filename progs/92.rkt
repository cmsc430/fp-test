#lang racket
(cond
 (5
  (if (cond
       (else
        (if (abs (if (zero? #t) (if #t #t #f) (integer->char #\胴))) #\킈 -3)))
    #t
    #f))
 (else #f))
