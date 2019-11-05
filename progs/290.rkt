#lang racket
(integer->char
 (if -4
   (cond
    (-2 #\똙)
    (514 4)
    (-2 #t)
    (#\U0009DED4 (add1 #\포))
    (-1 11)
    ((if #f #\U0003C051 -1) #\ꨲ)
    (#\与 (if #f #f #f))
    (#t #\𧟀)
    (else (cond (3 #f) (else #t))))
   (- (char->integer #t))))
