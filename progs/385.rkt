#lang racket
(let ((x (gensym)))
  (eq? x x))
