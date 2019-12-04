#lang racket
;; Garden fence encryption benchmark
;; http://lists.racket-lang.org/users/archive//2009-March/031274.html

(begin
  (define (read-string)
    (list->string (read-chars)))

  (define (read-chars)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (cons c (read-chars)))))

  (define (begin-void . _)
    (void))
  
  (define (> n m)
    (< m n))

  (define (substring s i j)
    (letrec ((substring/acc
              (λ (i a)
                (if (= i j)
                    (list->string (reverse a))
                    (substring/acc (add1 i) (cons (string-ref s i) a))))))
      (substring/acc i '())))

  (define (string-set str k char)
    (string-append (substring str 0 k)
                   (string-append (list->string (list char))
                                  (substring str (add1 k) (string-length str)))))
  
  (define (build-list n f)
    (letrec ((build-list/acc
              (λ (n a)           
                (cond [(zero? n) a]
                      [else (build-list/acc (sub1 n) (cons (f n) a))]))))
      (build-list/acc n '())))

  (define (make-list n x)
    (build-list n (λ (i) x)))

  (define (list-update xs i f)
    (cond [(zero? i) (cons (f (car xs)) (cdr xs))]
          [else (cons (car xs) (list-update (cdr xs) (sub1 i) f))]))

  ;; app-rev is (compose append reverse)
  (define (app-rev sl ls)
    (cond [(empty? sl) ls]
          [else (app-rev (cdr sl) (cons (car sl) ls))]))

  ;; ----------------------------------------------------
  ;; Shared

  ;; String [Listof Nat] -> String
  ;; Permute the string according to the given permutation.
  (define (permute str perm)
    (permuter str perm
              (λ (i j) (list i j))))

  ;; String [Listof Nat] -> String
  ;; Unpermute the string according to the given permutation.
  (define (unpermute str perm)
    (permuter str perm
              (λ (i j) (list j i))))

  ;; String [Listof Nat] [Nat Nat -> [List Nat Nat]] -> String
  ;; Abstraction of permute/unpermute.
  (define (permuter str perm f)
    (letrec ((loop
              (λ (ans i p)
                (cond [(= i (string-length str)) ans]
                      [else (loop (string-set ans
                                              (car (f i (car p)))
                                              (string-ref str
                                                          (car (cdr (f i (car p))))))
                                  (add1 i)
                                  (cdr p))]))))
      (loop str 0 perm)))
   

  ;; ----------------------------------------------------
  ;; Functional random access list solution


  ;; Nat Nat -> [Listof Nat]
  ;; Generate a fence permutation of the given
  ;; height (> 1) for strings of length len.
  (define (fence-ra height len)
    (let ([bot 0]
          [top (sub1 height)])
      (letrec ((loop
                (λ (n level move rls)   
                  (cond [(= n len) 
                         (foldr app-rev '() rls)]
                        [(< level bot) (loop n (add1 bot) (λ (x) (add1 x)) rls)]
                        [(> level top) (loop n (sub1 top) (λ (x) (sub1 x)) rls)]
                        [else
                         (loop (add1 n) 
                               (move level) 
                               move
                               (list-update rls level (λ (ls) (cons n ls))))]))))
        (loop 0 0 (λ (x) (add1 x)) (make-list height '())))))

  ;; String Nat -> String
  (define (encrypt-ra text height)
    (permute text (fence-ra height (string-length text))))

  ;; String Nat -> String
  (define (decrypt-ra text height)
    (unpermute text (fence-ra height (string-length text))))

  (let ((s (read-string)))
    (let ((es (encrypt-ra s 6)))
      (let ((a (display es)))
        (let ((b (display "\n")))
          (let ((c (display (decrypt-ra es 6))))
            (display "\n")))))))
