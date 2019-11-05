#lang racket
(let ((s2
       (let ((z2
              (cond
               (#f #f)
               ((char->integer #\ً) -8)
               (-1 -1)
               (#\閧 (integer? #t))
               (#t (if #t -1 #\爿))
               (#\킑 #t)
               (#\ڰ #t)
               (-8 (let ((q2 #\ꋛ)) -1))
               (#f 16)
               (else (add1 (char? #t))))))
         (integer->char #\↸))))
  #t)
