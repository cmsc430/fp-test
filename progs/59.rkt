#lang racket
(if (cond
     ((let ((q1 (if #\h #t (zero? #\U00090ACD)))) #f)
      (cond
       ((if #t #f #f) #t)
       ((let ((g0 (integer->char #\꺌))) -2) (if #f #t 2))
       (#f
        (cond
         (#\䳌 0)
         ((if -2 -6 #\킩) #\U0005C562)
         (-2 #t)
         (#f 5)
         (#\U0001343B #f)
         (else #\U000651CE)))
       ((cond (else 4)) (if 7 0 -1))
       ((let ((g1 #\U0007198E)) #f) (let ((a2 #t)) a2))
       ((let ((u0 #t)) #\퇈) #t)
       (#\U0010AF38 (cond (#t -2) (#f #f) (else #t)))
       (-4 (cond (3 3) (#f #t) (else #\疦)))
       (else (let ((s0 #f)) #t))))
     ((add1 #t) #f)
     (#f (boolean? -1))
     (else #\Ꚅ))
  #t
  #\U000D3B70)
