#lang racket
(begin
  (define (* n m)
    (match n
      [0 0]
      [1 m]
      [n (+ m (* (sub1 n) m))]))

  (define (expt n m)
    (match m
      [0 1]
      [1 n]
      [m (* n (expt n (sub1 m)))]))

  (define (fact n)
    (match n
      [0 1]
      [n (* n (fact (sub1 n)))]))

  (define (read-number)
    (digits->number (read-digits)))

  (define (read-digits)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (if (eq? #\newline c)
              '()
              (cons c (read-digits))))))

  (define (digits->number ds)
    (match ds
      ['() 0]
      [(cons d ds)
       (+ (* (digit->number d)
             (expt 10 (length ds)))
          (digits->number ds))]))

  (define (digit->number d)
    (match d
      [#\0 0]
      [#\1 1]
      [#\2 2]
      [#\3 3]
      [#\4 4]
      [#\5 5]
      [#\6 6]
      [#\7 7]
      [#\8 8]
      [#\9 9]))

  (fact (read-number)))
