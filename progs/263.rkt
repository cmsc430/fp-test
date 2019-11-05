#lang racket
(let ((l1 #t))
  (cond
   (#t -7)
   ((cond
     (#t #f)
     ((let ((o1 3)) #t) 2)
     (#f (char->integer #f))
     (else (let ((c1 l1)) -3)))
    (let ((q0 (if #f #f #t))) q0))
   (#t
    (cond
     ((sub1 l1) (cond (else 2)))
     (else (cond (-11 #f) (#\舟 #t) (#f #f) (#f l1) (-16 l1) (else -2)))))
   (else
    (let ((n0 (let ((q2 #f)) l1)))
      (cond (-4 #f) (#f #t) (#t 16) (#t #f) (#\㗕 #f) (else #f))))))
