#lang racket
(cond
 (#\ꠦ
  (if (cond
       (#t (boolean? #t))
       (#t (cond (else -1)))
       (#\U000EEBE9 0)
       (else (cond (else #\⚸))))
    (abs (if 3 #\絬 #t))
    2))
 (-3 (zero? 7))
 (else
  (if #t
    #\Ꞧ
    (if (char->integer 1)
      #t
      (cond
       (#t #t)
       (#t #\ⰴ)
       (#f #f)
       (-3 0)
       (-1 2)
       (#t #\U00059FF0)
       (#f #\푍)
       (2 #f)
       (#\U000A629B #\U00092C95)
       (else 7))))))
