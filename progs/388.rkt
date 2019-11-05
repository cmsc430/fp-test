#lang racket
(let ((p (cons (gensym) (gensym))))
  (eq? (car p) (car p)))
