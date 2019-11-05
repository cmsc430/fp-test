#lang racket
(char?
 (cond
  (#\U00105055 (if #\㚰 #f #\U00037A4A))
  (#\읔 #\U000DE72C)
  (else (let ((e2 (if #t #\繑 #\U000D106E))) (if 5 #t -3)))))
