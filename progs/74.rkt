#lang racket
(integer? (if #t #t (let ((z1 (integer->char #\譊))) #\棘)))
