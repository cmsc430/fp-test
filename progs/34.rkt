#lang racket
(let ((y2 (boolean? (let ((e1 #t)) (cond (else e1))))))
  (let ((a2 (cond (#t (if #\兀 y2 4)) (else #\둝)))) a2))
