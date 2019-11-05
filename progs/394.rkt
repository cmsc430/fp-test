#lang racket
(begin
  (define (string->list s)
    (string->list/a s (string-length s) '()))

  (define (string->list/a s i cs)
    (cond [(zero? i) cs]
          [else (string->list/a s (sub1 i)
                                (cons (string-ref s (sub1 i)) cs))]))

  (define (list . xs) xs)

  ;; function form of and (not short-circuiting)
  (define (and . bs)
    (if (empty? bs)
        #t
        (if (car bs)
            (apply and (cdr bs))
            #f)))

  (define (not x) (if x #f #t))

  ;; type Regexp =
  ;; | 'zero
  ;; | 'one
  ;; | `(char ,Char)

  ;; Regexp String -> Boolean
  (define (accepts r s)
    (matcher r (string->list s) (λ (cs) (empty? cs))))

  ;; Regexp (Listof Char) ((Listof Char) -> Bool) -> Bool
  (define (matcher r cs k)
    (cond [(eq? r 'zero) #f]
          [(eq? r 'one) (k cs)]
          [(eq? (car r) 'char)
           (cond [(empty? cs) #f]
                 [else (and (char=? (car (cdr r)) (car cs)) (k (cdr cs)))])]
          [else 'impossible]))

  (and (accepts (list 'char #\a) "a")
       (not (accepts (list 'char #\a) "b"))
       (not (accepts (list 'char #\a) ""))))
