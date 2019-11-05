#lang racket
(let ((v2 #t))
  (if 4 #t (if #t (if #f v2 v2) (cond (#f v2) (#\밡 #\곀) (#f #f) (else -1)))))
