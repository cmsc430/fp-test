#lang racket
(let ((i0 (boolean? #f)))
  (let ((o2 (char? 1)))
    (cond
     (#f #\䭊)
     (#t #t)
     (#\U000FF6E6 #t)
     (o2 -5)
     (else
      (cond
       (#f #f)
       (#t #\U000E6370)
       (i0 i0)
       (#t #\겅)
       (#\U000D129D #t)
       (#t #t)
       (#\U00013BF9 o2)
       (#\U000A4FDE #\U0009D1BE)
       (-2 o2)
       (#f -1)
       (#\u318F #t)
       (else #t))))))
