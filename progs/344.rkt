#lang racket
(cond [(string? (make-string #\a 0)) #t]
      [else #f])
