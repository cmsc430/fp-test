#lang racket
(if -2 #t (let ((q1 -1)) (if #f #t q1)))
