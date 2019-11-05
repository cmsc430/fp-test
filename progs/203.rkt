#lang racket
(let ((s1
       (cond
        ((boolean? (char? #t)) #\U000746C6)
        ((cond (else #t)) #f)
        (#t (abs (if #t #f #t)))
        ((zero? 8) #\U000818BA)
        ((char->integer #\U00033576)
         (char->integer (boolean? (if #\뺞 #t #\𥠾))))
        (#f #t)
        (else (integer? #\U000D8932)))))
  (cond
   (#f (cond (else (if -5 #t s1))))
   (#\ॺ
    (cond
     (#\␄
      (cond
       (#f s1)
       (#t #f)
       (#t 1)
       (#\麒 -2)
       (#t 1)
       (#t #\U00100A19)
       (#\U00103CE5 s1)
       (#t s1)
       (else #t)))
     ((let ((v1 4)) #f) #f)
     (else (let ((f1 -6)) #f))))
   (#\U00095625 #\ꇳ)
   ((boolean? #t) (zero? s1))
   ((if #f (if 2 #t #\㬤) (let ((d2 s1)) #f)) (sub1 (let ((u2 s1)) 3)))
   (#\U000F1509 -4)
   (s1
    (cond
     (#f #\箜)
     (#\⭉ #t)
     (1 (let ((g1 #\慘)) #t))
     (#t
      (cond
       (#f #t)
       (#\U00105490 #\U0009096C)
       (#f #t)
       (#t #f)
       (#f #t)
       (else #\ḷ)))
     (#f s1)
     ((cond (s1 #t) (#t -33) (else -2)) -8)
     (s1 s1)
     (#t (cond (else 2)))
     (else #t)))
   ((let ((v2 s1)) v2) #\췖)
   (s1 #\U00086538)
   (else s1)))
