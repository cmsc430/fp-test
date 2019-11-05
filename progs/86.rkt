#lang racket
(-
 (abs
  (cond
   (#t (integer? #f))
   (#\𧼭 #\솔)
   (0 #f)
   ((if #t #\U0004CBEF #f) (if -1 #t #\U0001CC4E))
   ((cond (else #f)) #t)
   (else 4))))
