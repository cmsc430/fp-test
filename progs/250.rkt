#lang racket
(cond
 (#f (integer? #t))
 (else (cond (#\䪘 #f) (else (if (if -1 #t #\䐳) (char? #f) #f)))))
