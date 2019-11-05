#lang racket
(let ((x 8) (y 9) (z 10))
  (cond [(<= y x) (abs #f)]
        [else (< y z)]))
