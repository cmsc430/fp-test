#lang racket
(cond
 (#f (cond (else #\u2069)))
 (#t 1)
 ((let ((h0 2)) h0) (cond (#f (char? 8)) (else (zero? (if #\捻 #f #\丄)))))
 ((if (char? #f) #f (add1 #t)) #\U0008A1BB)
 (else (let ((c2 (let ((s0 (sub1 #f))) (if 1 #\U000CAAA5 -1)))) (sub1 c2))))
