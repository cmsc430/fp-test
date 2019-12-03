#lang racket

;; Reads in a newline separated list of binary numbers and sorts them
;; in ascending order using insertion sort

(begin
  (define (read-binary-numbers)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (cons (bits->number (read-binary-number c))
                (read-binary-numbers)))))

  (define (read-binary-number c)
    (let ((cn (read-char)))
      (if (char=? #\newline cn)
          (list c)
          (cons c (read-binary-number cn)))))

  (define (bits->number bs)
    (match bs
      ['() 0]
      [(cons b r)
       (+ (arithmetic-shift (bit->number b) (length bs))
          (bits->number r))]))

  (define (bit->number b)
    (match b
      [#\1 1]
      [#\0 0]))
 
  (define (sort-asc ns)
    (match ns
      ['() '()]
      [(cons n ns)
       (insert n (sort-asc ns))]))
 
  (define (insert n sns)
    (match sns
      ['() (list n)]
      [(cons m sns)
       (if (< n m)
           (cons n (cons m sns))
           (cons m (insert n sns)))]))

  (sort-asc (read-binary-numbers)))
