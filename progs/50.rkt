#lang racket
(let ((o1 (if #f #f #f))) (integer->char (boolean? (if -2 #t #f))))
