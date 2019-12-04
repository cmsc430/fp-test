#lang racket
(begin
  (define (read-binary-numbers)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (cons (bits->number (read-bits c))
                (read-binary-numbers)))))

  (define (read-bits c)
    (let ((cn (read-char)))
      (if (eq? cn #\newline)
          (list c)
          (cons c (read-bits cn)))))

  (define (bits->number bs)
    (match bs
      ['() 0]
      [(cons b bs)
       (+ (arithmetic-shift (bit->number b) (length bs))
          (bits->number bs))]))

  (define (bit->number b)
    (match b
      [#\0 0]
      [#\1 1]))
          
  (length (remove-duplicates (read-binary-numbers))))
