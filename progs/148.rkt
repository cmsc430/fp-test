#lang racket
(if (integer? (char? #t))
  (cond (2 #\U000A48B0) (else (if #t #\U0006F0D9 #t)))
  (cond (else #f)))
