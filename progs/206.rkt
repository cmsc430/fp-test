#lang racket
(if (cond
     ((cond
       (#t (let ((l1 #f)) l1))
       ((cond
         (#\U00075A47 #\靠)
         (#f #\U0001EA14)
         (-3 #\鞞)
         (#f #f)
         (#\U00015A22 #f)
         (1 #f)
         (#f #\U00062EE7)
         (#f #t)
         (#f -8)
         (#f #\U000333C6)
         (3 #\䊢)
         (else #f))
        (let ((s0 1)) 1))
       (else (if #t #\㙿 2)))
      (if #t #t -9))
     (#t (abs #\U000B6B12))
     (else
      (cond
       (#t
        (boolean?
         (cond
          ((if #t #\U00063EE1 #t)
           (cond (-2 #t) (#t #f) (#\U00012CBC #f) (else #\ᒆ)))
          (#t (char? #\瑇))
          (#f (cond (#t #f) (else -2)))
          (#\ꨮ #f)
          (else (if #\㓢 #t #\癚)))))
       (#t (if 4 (if #t #\U00099DB7 2) (sub1 (if #f #\U000E2D91 #t))))
       (else #\౻))))
  #t
  1)
