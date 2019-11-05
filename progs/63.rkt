#lang racket
(integer->char (if (if #\𨴮 (char? #\U0001EE65) #f) #t #\U00030D7D))
