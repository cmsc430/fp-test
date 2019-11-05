#lang racket
(if #\uF697
  (if 2 3 (boolean? #f))
  (cond
   ((let ((m1 #t)) (abs #\U000AD1BC)) #t)
   ((if (let ((k2 #f)) #f) -5 #f) (if #\鶰 (integer->char -4) (if 1 #f 16)))
   (#t
    (if #\촕
      (cond
       (#f #f)
       ((-
         (if #t
           #\豭
           (cond (else (cond (#t #\䨆) (#t #f) (#f #\艴) (#f #\흶) (else #\⋥))))))
        #\U000CF86A)
       (#f #t)
       (#\U00098BE5 #f)
       (else -1))
      (cond
       (-1 0)
       (5 #\U000CF984)
       (-3 #t)
       (#\U00080589 #\U0003FFEF)
       (2 -33)
       (#t #t)
       (else #f))))
   (-4 (let ((b1 (cond (4 #f) (else 7)))) #\渃))
   (#\ᵱ (let ((f1 #t)) -7))
   (else #\U000C549E)))
