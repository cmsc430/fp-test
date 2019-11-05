#lang racket
(let ((l2
       (if (cond
            (#f (cond (-2 9) (#\U000EB623 #t) (2 #t) (#f #f) (else #f)))
            (else (if #\땣 #t 4)))
         -3
         #f)))
  (let ((y0 -3)) (let ((x1 (cond (y0 -4) (else #t)))) #t)))
