#lang racket
(cond [(<= 3 4) (string-ref "fred" 4)]
      [else #\c])

