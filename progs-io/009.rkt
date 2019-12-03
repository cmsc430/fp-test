#lang racket

;; ----------------------------------------------------
;; Felleisen, output data driven design recipe solution 
(begin
  (define (read-string)
    (list->string (read-chars)))
  
  (define (read-chars)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (cons c (read-chars)))))

  (define (>= n m)
    (<= m n))

  (define (make-list n x)
    (build-list n (λ (i) x)))
  
  (define (build-list n f)
    (letrec ((build-list/acc
              (λ (n a)           
                (cond [(zero? n) a]
                      [else (build-list/acc (sub1 n) (cons (f n) a))]))))
      (build-list/acc n '())))

  (define (map2 f xs ys)
    (match xs
      ['() '()]
      [(cons x xs)
       (cons (f x (car ys))
             (map2 f xs (cdr ys)))]))

  (define (sort ns cmp)
    (match ns
      ['() '()]
      [(cons n ns)
       (insert cmp n (sort ns cmp))]))
 
  (define (insert cmp n sns)
    (match sns
      ['() (list n)]
      [(cons m sns)
       (if (cmp n m)
           (cons n (cons m sns))
           (cons m (insert cmp n sns)))]))

  (define (filter p xs)
    (match xs
      ['() '()]
      [(cons x xs)
       (if (p x)
           (cons x (filter p xs))
           (filter p xs))]))

  (define (take xs i)
    (match i
      [0 '()]
      [i (cons (car xs)
               (take (cdr xs)
                     (sub1 i)))]))

  (define (drop xs i)
    (match i
      [0 xs]
      [i (drop (cdr xs)
               (sub1 i))]))
  
  (define (X) '_)

  (define (encrypt-dr str h)
    (list->string (fence-dr (string->list str) h)))

  ;; [Listof X] -> [Listof X]
  (define (fence-dr lox h)
    (let ((a (apply append (transpose (waves lox h)))))
      (filter (λ (e) (not (eq? (X) e))) a)))

  ;; [Listof X] Nat -> [Listof [Listof (U X Char)]]
  ;; chop the list into as many pieces of length h, plus padding of the 
  ;; last one
  (define (waves str h)
    (letrec ((down
              (λ (str)
                (cond
                  [(>= h (length str)) (list (fill h str))]
                  [else (cons (take str h) (up (drop str h)))])))
             (up
              (λ (str)
                (cond
                  [(>= (- h 2) (length str)) (list (pad (fill (- h 2) str)))]
                  [else (cons (pad (take str (- h 2))) (down (drop str (- h 2))))])))
             (pad
              (λ (str)
                (append (list (X)) (append (reverse str) (list (X))))))
             (fill
              (λ (h str) (append str (make-list (- h (length str)) (X))))))
      (down str)))

  ;; [Listof [Listof X]] -> [Listof [Listof X]]
  ;; transpose the matrix
  (define (transpose m)
    (cond
      [(empty? (car m)) '()]
      [else (cons (map (λ (x) (car x)) m) (transpose (map (λ (x) (cdr x)) m)))]))

  (define (decrypt-dr str h)
    (let ((e (fence-dr (build-list (string-length str) (λ (i) i)) h)))
      (let ((x (map2 list e (string->list str))))
        (let ((y (sort x (λ (i j) (<= (car i) (car j))))))
          (let ((z (map (λ (x) (second x)) y)))
            (list->string z))))))

  
  (let ((s (read-string)))
    (let ((es (encrypt-dr s 6)))
      (let ((a (display es)))
        (let ((b (display "\n")))
          (let ((c (display (decrypt-dr es 6))))
            (display "\n")))))))
  

