#lang racket
(let ((t0
       (integer->char
        (if (cond
             ((boolean? #t) (cond (#t #\U00100FA9) (else #t)))
             ((sub1 #t) #t)
             ((zero? #\硘) #f)
             ((- 2) (if #\루 #t #\澈))
             (#\뀄 -7)
             (#\U00106FA3 (boolean? #\U00035043))
             (#t #\U000FCC67)
             (#\U000BCA32 (cond (#\輺 #f) (#\U000FC9EA #t) (else #t)))
             (else (if #\U0008E7EC #\恼 #f)))
          (if -1 (if #f -1 -1) -1)
          0))))
  (cond (else -2)))
