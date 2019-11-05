#lang racket
(-
 (cond
  ((zero? (cond (#t #\U000DF749) (#t 4) (#t -4) (#f #f) (else -4))) #\㐵)
  ((cond
    (#\U0006CA61 (let ((x0 #\⟟)) 64))
    (#\喒 (char? #\実))
    ((cond (#f #f) (-1 #\둁) (else #\猤)) #\U000F3CDF)
    ((cond
      (2 #\U000712D6)
      (-2 -1)
      (#t
       (cond
        ((char? (if (if #t #f -4) (boolean? #f) #\䧹)) (char? #t))
        (else (boolean? 17))))
      (#t #f)
      (2 #f)
      (else -257))
     #\U0007ED32)
    (#\U000C2124
     (let ((r2 (cond (#t #\𠦴) (#\u9FF8 1) (else #\U000E1CD5)))) #f))
    (else (if #\U000946F2 -3 -1)))
   -1)
  (else #f)))
