#lang racket
(char->integer (let ((k0 #\佣)) (cond (9 (if k0 #f #t)) (k0 #f) (else k0))))
