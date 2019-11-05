#lang racket
(let ((f (λ () 'fred)))
  (eq? (f) (f)))
