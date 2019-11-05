#lang racket
(integer?
 (abs
  (cond (#\틩 #f) ((cond (#\U000A588F #f) (else #t)) #f) (else (if #f 8 #f)))))
