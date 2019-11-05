#lang racket
(cond
 ((cond
   (#t (cond (#\埬 #f) (else #f)))
   (#\U000C634B (let ((f0 (add1 2))) (if f0 #f #\U0005C480)))
   (else (sub1 6)))
  #t)
 (else (if #\້ #\U0006742A #\U0004196D)))
