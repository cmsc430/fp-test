#lang racket
(cond
 (#t -2)
 (#\U00076AD5 #\㣱)
 (#\U0001E7F2 (if 4 (if #\U000726F5 #f #f) #t))
 ((cond
   (#f -2)
   (-1
    (cond
     ((if -4 #f #f) (integer? -5))
     (#\븾 (char->integer #\U000AED73))
     (4 2)
     (else #f)))
   (else -4))
  (if #\U0003A198 5 2))
 (else #\U0006F496))
