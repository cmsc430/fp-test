#lang racket
(cond
 (#f #\婒)
 (#t #t)
 ((cond
   (#t #t)
   (#t (cond (-1 (char->integer #\U0002D615)) (else (if #t -7 #f))))
   (else (cond (#f (if #\U0001575A #f #t)) (else #t))))
  -8)
 (else
  (cond
   (#f (if #f (abs #f) #t))
   ((if #f #\ꡇ #f) #t)
   (#\U000B358B #\𦆺)
   (#f 7)
   ((if (if #\䲀 #f #f) #t -1)
    (if #\U00066900 #\벮 (cond (#f #t) (#t #t) (-5 -17) (else #\U000CFC2F))))
   (#f #t)
   (#\U000AA7D8 (if -2 -3 (if -1 #t #\U00075509)))
   (else (if #f #t (if #f #t #f))))))
