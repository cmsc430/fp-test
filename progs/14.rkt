#lang racket
(cond ((integer->char #f) #f) (#\U000BF254 #f) (else #t))
