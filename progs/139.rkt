#lang racket
(cond
 ((cond
   (#t #f)
   ((cond
     ((if #\渇 (cond (#\綤 #f) (else #\脏)) (if 5 #f -2)) (abs (cond (else #\膥))))
     (else (- #f)))
    (abs (cond (else (if #f #\㪜 #\؈)))))
   (#f #\U00050482)
   (#\U000BB419 (boolean? (char->integer #\𩈡)))
   ((abs 2) (cond (-64 (if (char? #\U000E3763) 256 #\彉)) (else #f)))
   (-2 (integer? #\땿))
   (else
    (if (cond (else (if -2 #f #\U000F3A99)))
      (cond
       ((char? #\♄) #\ㄘ)
       ((if #f #f #\㲮) (integer? #t))
       ((if #\Չ #f #t) (char? #f))
       (else 10))
      #t)))
  #t)
 (else
  (char->integer
   (let ((s1
          (abs
           (if (if (boolean? #\䦝) #f 2)
             (if (cond (#t #\U00011E62) (else #t)) (- #\U00074427) #\U0010C682)
             (sub1 #\敶)))))
     (if s1 #\奇 #\U0002ECA5)))))
