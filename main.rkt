#lang racket
(require rackunit)
(require racket/sandbox)

(define eval
  (let ((ev (make-evaluator 'racket)))
    (lambda (x)
      (with-handlers ([exn:fail? (lambda (_) 'err)])
        (ev x)))))

(define (read-prog p)
  (regexp-match "^#lang racket" p)
  (read p))

;; Code for submission needs to be in ".." directory
(require (only-in "../compile.rkt" compile)
         (only-in "../asm/interp.rkt" asm-interp asm-interp/io)
         (only-in "../syntax.rkt" prog? expr? closed?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax tests

(check-true (expr? 7))
(check-true (expr? "asdf"))
(check-true (expr? ""))
(check-true (expr? #t))
(check-true (expr? #t))
(check-true (expr? #\a))
(check-true (expr? '(add1 #f)))
(check-true (expr? '(sub1 #f)))
(check-true (expr? '(abs #f)))
(check-true (expr? '(- #f)))
(check-true (expr? '(zero? #f)))
(check-true (expr? '(integer->char #f)))
(check-true (expr? '(char->integer #f)))
(check-true (expr? '(char? #f)))
(check-true (expr? '(integer? #f)))
(check-true (expr? '(boolean? #f)))
(check-true (expr? '(box "adsf")))
(check-true (expr? '(+ 1 2)))
(check-true (expr? '(- 1)))
(check-true (expr? '(- 1 2)))
(check-true (expr? 'x))
(check-true (expr? '(let () x)))
(check-true (expr? '(let ((x 1)) x)))
(check-true (expr? '(let ((x 1) (y 2)) x)))
(check-true (expr? '(let ((x 1) (y 2) (z 3)) x)))
(check-true (expr? '(string-length "asdf")))
(check-true (expr? '(string-ref "asdf" 0)))
(check-true (expr? '(= #f #f)))
(check-true (expr? '(< #f #f)))
(check-true (expr? '(string? #f)))
(check-true (expr? '(box? #f)))
(check-true (expr? '(empty? #f)))
(check-true (expr? '(cons? #f)))
(check-true (expr? '(unbox #f)))
(check-true (expr? '(car #f)))
(check-true (expr? '(cdr #f)))
(check-true (expr? '(make-string #f #f)))
(check-true (expr? '(= #f #f)))
(check-true (expr? '(< #f #f)))
(check-true (expr? '(<= #f #f)))
(check-true (expr? '(char=? #f #f)))
(check-true (expr? '(boolean=? #f #f)))
(check-true (expr? '(+ #f #f)))
(check-true (expr? '(- #f #f)))

(check-false (expr? '(let 1)))
(check-false (expr? '(let x 1)))
(check-false (expr? '(let x y 1)))
(check-false (expr? '(let (x y) 1)))
(check-false (expr? '(let ((x)) 1)))
(check-false (expr? '(let ((1 2)) 1)))
(check-false (expr? '(let ((abs 1)) 1)))
(check-false (expr? '(let ((string-ref 1)) 1)))
(check-false (expr? '(let ((+ 1)) 1)))
(check-false (expr? '(let ((string? 1)) 1)))
;(check-false (expr? '(1)))
(check-false (expr? '(box)))
(check-false (expr? '(string-ref "asdf")))
(check-false (expr? '(+ 1 2 3)))
(check-false (expr? '(make-string #f)))
(check-false (expr? '(make-string #f #f #f)))

(check-true (prog? 5))
(check-true (prog? '(begin (define (f x) x)
                           (f 5))))
(check-true (prog? '(begin (define (f x y z) x)
                           (f 5 6 7))))
(check-true (prog? '(begin (define (f x y z) (g x))
                           (define (g q) q)
                           (f 5 6 7))))

(check-false (prog? '(begin (define (f f) f)
                            (f 5))))



(check-true (closed? 7))
(check-true (closed? "asdf"))
(check-true (closed? ""))
(check-true (closed? #t))
(check-true (closed? #f))
(check-true (closed? #\a))
(check-true (closed? '(box "adsf")))
(check-true (closed? '(+ 1 2)))
(check-true (closed? '(- 1)))
(check-true (closed? '(- 1 2)))
(check-true (closed? '(let ((x 1)) x)))
(check-true (closed? '(let ((x 1) (y 2)) x)))
(check-true (closed? '(let ((x 1) (y 2) (z 3)) x)))
(check-true (closed? '(string-length "asdf")))
(check-true (closed? '(string-ref "asdf" 0)))
(check-true (closed? '(let ((x 1) (y 2))
                        (let ((z y))
                          (+ x z)))))

(check-false (closed? 'x))
(check-false (closed? '(let () x)))
(check-false (closed? '(let ((x 1)) y)))
(check-false (closed? '(let ((x 1) (y x)) y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler tests

(define (run p)
  (asm-interp (compile p)))

(check-equal?
 (run '(let ((x 1)) x))
 (eval '(let ((x 1)) x)))

(check-equal? (run
               '(begin (define (f x) x)
                       (f 5)))
              5)

(check-equal? (run
               '(begin (define (tri x)
                         (if (zero? x)
                             0
                             (+ x (tri (sub1 x)))))
                       (tri 9)))
              45)

(check-equal? (run
               '(begin (define (even? x)
                         (if (zero? x)
                             #t
                             (odd? (sub1 x))))
                       (define (odd? x)
                         (if (zero? x)
                             #f
                             (even? (sub1 x))))
                       (even? 101)))
              #f)

(check-equal? (run
               '(begin (define (map-add1 xs)
                         (if (empty? xs)
                             '()
                             (cons (add1 (car xs))
                                   (map-add1 (cdr xs)))))
                       (map-add1 (cons 1 (cons 2 (cons 3 '()))))))
               ''(2 3 4))

(check-equal? (run
               '(begin
                  (define (explode str)
                    (explode/i str 0))
                  (define (explode/i str i)
                    (if (= (string-length str) i)
                        '()
                        (cons (string-ref str i)
                              (explode/i str (add1 i)))))
                  (explode "fred")))
              ''(#\f #\r #\e #\d))

