#lang racket
(sub1
 (if #f
   #\糙
   (if #t
     (cond (#f #t) (#\𢷖 #f) (0 #\U000EE61A) (#t (boolean? -2)) (else #\㾊))
     8)))
