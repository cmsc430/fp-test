#lang racket
(cond
 (else
  (cond
   ((if (cond (#\᳠ #\뻩) (#f #\U00097481) (else #f)) -2 2) #t)
   (#f (if 3 #\褑 (char->integer #t)))
   ((add1 #t)
    (let ((q0
           (char->integer
            (abs
             (cond
              (#f #t)
              (#t (boolean? #f))
              (#\U0003D129 5)
              (#t #f)
              (#f (if #t (if #f #\𧁎 #f) (integer->char #f)))
              (-1 (abs (cond (#\U00057960 #\땤) (0 #t) (else #t))))
              (else (if #\U000957E5 3 (if 2 1 #\U0005BDF9))))))))
      (if #t #t q0)))
   ((if (add1 (if 1 (- #\抎) #f)) (if #\U000B352F #t 3) #t) (char? (char? #t)))
   (else (cond (#t #f) (else 0))))))
