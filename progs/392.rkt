#lang racket
(begin
  (define (string->list s)
    (string->list/a s (string-length s) '()))
  
  (define (string->list/a s i cs)
    (cond [(zero? i) cs]
          [else (string->list/a s (sub1 i)
                                (cons (string-ref s (sub1 i)) cs))]))
  
  ;; type Regexp =
  ;; | 'zero
  ;; | 'one
  
  ;; Regexp String -> Boolean
  (define (accepts r s)   
    (matcher r (string->list s) (λ (cs) (empty? cs))))
  
  ;; Regexp (Listof Char) ((Listof Char) -> Bool) -> Bool
  (define (matcher r cs k)
    (cond [(eq? r 'zero) #f]
          [(eq? r 'one) (k cs)]
          [else 'impossible]))
  
  (accepts 'one "fred"))
