#lang racket
(cond
 ((if #f
    1
    (if (cond (else #f))
      (cond (#f #f) (#\U00067CC6 #\U00016FDF) (else 1))
      (if #f -17 #\⻣)))
  #f)
 (else #f))
