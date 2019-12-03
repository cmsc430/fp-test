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

  (define (build-list n f)
    (letrec ((build-list/acc
              (λ (n a)
                (cond [(zero? n) a]
                      [else (build-list/acc (sub1 n) (cons (f (sub1 n)) a))]))))
      (build-list/acc n '())))
  
  (define (sublists xs)
    (letrec ((sublists*
              (λ (xs)
                (cond
                  [(empty? xs) (list '())]
                  [(empty? (cdr xs)) (list xs)]
                  [else
                   (cons xs
                         (append (sublists* (cdr xs))
                                 (sublists* (drop-last xs))))]))))
      (remove-duplicates (sublists* xs))))
            
  (define (drop-last xs)
    (match xs
      [(cons x '()) '()]
      [(cons x xs)
       (cons x (drop-last xs))]))

  (define (longest-common xs ys)
    (match xs
      ['() '()]
      [(cons x xs)
       (if (member x ys)
           (longest-common/acc x xs ys)
           (longest-common xs ys))]))

  (define (longest-common/acc x xs ys)
    (match xs
      ['() x]
      [(cons x* xs)
       (if (< (length x*) (length x))
           (longest-common/acc x xs ys)
           (if (member x* ys)
               (longest-common/acc x* xs ys)
               (longest-common/acc x xs ys)))]))

  (let ((n (read-number)))
    (longest-common (sublists (build-list n (λ (x) x)))
                    (sublists (build-list n (λ (x) (add1 x)))))))


