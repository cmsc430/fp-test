#lang racket
(cond
 ((char->integer #t) -2)
 (#\魝
  (cond
   ((if (- #f)
      (cond
       (#\𧯏 -1)
       (#f #t)
       (#t 16)
       (#f #\͡)
       (8 #f)
       (-12 #t)
       (else #\U000F3687))
      (cond
       ((integer->char #f) #f)
       (#t -1)
       (#t #f)
       (#t 5)
       (#\剔 #\퓒)
       (-4 4)
       (#\U00062DFB 4)
       (else #f)))
    -1)
   ((if (if #f 2 (cond (-1 -2) ((- -3) 17) (else #f)))
      #\朾
      (cond
       (#f (boolean? -4))
       ((cond
         ((integer->char #f) (if -9 #f #\U0004A9EC))
         (11 #f)
         (#f 1)
         (#f (if -1 #\U0010DDEC #\옮))
         (else (sub1 #f)))
        (if #t #f #f))
       (#f #\U0007D70F)
       ((if (if #t #t 0) (char? #\U00096780) (cond (#f -3) (else #t))) #f)
       (else
        (cond
         ((add1 8) (cond (else #\Ꮪ)))
         ((cond (else #f)) #f)
         ((if #\U000D26B4 #\U00094B50 -5) #f)
         (#f (add1 #t))
         ((cond (#t #t) (#t #f) (else -1)) (- #f))
         ((cond
           (#\ꈷ #t)
           (#f #f)
           (#\쮇 #f)
           (#\ሓ #f)
           (#\𨶆 #\U000B2671)
           (#\ꆙ 16)
           (else #t))
          (char->integer #t))
         (#\˄ (add1 #f))
         ((sub1 #\U00035780) -3)
         (else #t)))))
    #f)
   (#t
    (cond
     ((let ((q1 #f)) q1) (if #f 32 (integer? (abs #\憜))))
     (else (add1 #\U0010F7F3))))
   (#f (char->integer #t))
   ((cond (else #t)) (cond (-2 (if #f #f #\U000D7A2E)) (else #f)))
   (else #\氭)))
 (3 -6)
 ((if (cond (else #t)) (if #t (if #\U000F9AB8 -1 #\柭) (cond (else -8))) #t)
  (let ((d0 (if #f #f (add1 #f)))) #\안))
 (else (let ((h2 1)) #t)))
