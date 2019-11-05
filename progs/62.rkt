#lang racket
(let ((v1
       (cond
        ((cond (else (cond (else #t)))) (add1 #\U000455AF))
        (else (if #t #t (cond (else #t)))))))
  #f)
