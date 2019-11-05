#lang racket
(if (abs #f) 1 (if (cond (else #t)) 1 (if -5 (if 1 (if #t 10 2) -4) (abs 0))))
