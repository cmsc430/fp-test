#lang racket
(cond
 (1 (if 4 8 #t))
 ((cond
   (#\U000BDCEB #\U000F2428)
   (#f #\U00041C27)
   (#\瘖 #t)
   ((if #f #f #\ꪐ) #f)
   ((boolean? 8) (if #t (integer? #t) #\U00058C96))
   (-2 #t)
   (3 (boolean? (cond (else #t))))
   ((char->integer (sub1 -2)) #f)
   (else #\칵))
  (if #t -1 1))
 (2
  (if (integer->char (cond (-2 #\U00099527) (#t 6) (else -2)))
    (if #\鱉
      (cond (#\㈎ #t) (#t #f) (else #f))
      (cond (#\U000BD5C3 #f) (#\U0009ECB7 #\䵨) (#f #\臛) (else 3)))
    #t))
 ((cond
   ((integer? #f) #t)
   (#\參
    (if (if #\䰂 #f #\U000909C2)
      (cond (#t 2) (2 #f) (#t #\♸) (else 2))
      (integer->char 2)))
   (-8 2)
   ((cond
     (#f (integer->char #\u0D54))
     ((char? #f)
      (cond
       (-4 4)
       (#f #\U000A5DB9)
       (#\U000BD96F #\둦)
       (#t #f)
       (#f #t)
       (#t #\苯)
       (#f -1)
       (#f #t)
       (#t #t)
       (#\䐼 #t)
       (0 #f)
       (#\U000AF1DD #t)
       (#f #t)
       (else 0)))
     (#t #t)
     ((if #\U00047B6F #\U0005DFCF #t) #t)
     (#\U000AC9FC #\U00059442)
     ((sub1 #f) #t)
     ((cond (#\阠 #t) (else -4)) (integer->char 4))
     ((if #t #\㺖 #f) #t)
     (#f (boolean? #t))
     (#f #f)
     (else (char->integer -3)))
    -4)
   (else #f))
  2)
 (4
  (if #t
    #t
    (zero?
     (cond (#\u9FF4 #t) (#\U000B6454 #\謿) (#f -3) (4 #t) (#f #t) (else 9)))))
 ((if (if #f (zero? 0) (abs #t))
    -1
    (cond ((cond (#t #f) (else #f)) #f) ((add1 #t) #f) (else (sub1 #t))))
  (if (if #\䷷ (boolean? #f) (integer? #t)) #\㹥 #t))
 (#t #\U000894DB)
 ((- (if (if #t -4 #f) #f #t)) (- #f))
 (else
  (cond
   (#t (if (cond (else -6)) (if #t #t #t) (zero? #t)))
   (2 (cond (-2 (sub1 #f)) ((cond (#f #\ꍼ) (else #f)) #t) (#f -2) (else #f)))
   ((abs #t) #t)
   (9 (integer->char (if #t 4 1)))
   ((cond (else (sub1 #\U00033774))) #\U000A5715)
   (#f (zero? (cond (else #f))))
   ((add1 #t) -3)
   (#t (cond (#f #t) (#t #f) (else (if #t #f #\Ȉ))))
   (else (integer? (integer->char #\U0010028C))))))
