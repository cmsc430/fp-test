#lang racket
(let ((l2
       (let ((b2
              (cond
               (#f
                (cond
                 (#\U000C8ABC 3)
                 (#f #t)
                 (#\푏 #f)
                 (#t #\U000B6671)
                 (#t (if #t #t #t))
                 (2 #\U00059072)
                 (#t (cond ((if #t #t -2) #\ꊗ) (else -3)))
                 (else (integer->char #t))))
               (else
                (cond
                 (#\u0A64 #\U00056490)
                 (#\U0010FF0A #t)
                 (#\U00100B43 #\泜)
                 (else #\U00012AA4))))))
         (abs (char? #t)))))
  (cond (l2 l2) (else #f)))
