#lang racket
(if 2
  (if (integer? 3)
    (cond
     (#\U000D3EBA (let ((z0 #f)) z0))
     (#f (if -4 #f #\ꧤ))
     ((cond
       (-4 #f)
       (#\u0010 #t)
       (#t 6)
       (#t #\U000BF5EA)
       (#t 1)
       (#f 5)
       (#t #\U00040977)
       (#\쟑 #\U0007CA8C)
       (else 4))
      #\U0002F07B)
     (else #\㴷))
    #f)
  #f)
