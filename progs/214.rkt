#lang racket
(if (boolean? #\윳)
  (abs (boolean? (cond (#\㝜 #t) (else #f))))
  (cond (2 (if -2 #f (- 1))) (else #\U000EAFD5)))
