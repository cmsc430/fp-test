#lang racket
(integer?
 (if #t
   (if (if #f -1 #\U000A17E8) (integer->char #f) #t)
   (cond
    ((if #\坕 #f 1) #t)
    (else
     (cond
      (2 4)
      (#\U0004EF24 #t)
      (-9 -4)
      (-4 #\U000B1473)
      (#t #\U000F2A2D)
      (#f 2)
      (#f #f)
      (else #t))))))
