#lang racket
(char=? (string-ref (make-string #\q 100) 99) #\q)
