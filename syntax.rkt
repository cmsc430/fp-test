#lang racket
(provide (all-defined-out))

;; Any -> Boolean
;; Is x a well-formed program?
(define (prog? x)
  (match x
    [(list 'begin ds ... e)
     (and (andmap defn? ds)
          (expr? e))]
    [_ (expr? x)]))

;; Any -> Boolean
(define (defn? x)
  (match x
    [`(define ,(list f xs ...) ,e)
     (and (variable? f)
          (andmap variable? xs)
          (unique? (cons f xs))
          (expr? e))]
    [`(define ,(list* f xs ... r) ,e)
     (and (variable? f)
          (variable? r)
          (andmap variable? xs)
          (unique? (cons r (cons f xs)))
          (expr? e))]
    [_ #f]))

;; (Listof Any) -> Boolean
(define (unique? xs)
  (match xs
    ['() #t]
    [(cons x xs)
     (and (not (memq x xs))
          (unique? xs))]))

;; Any -> Boolean
;; Is x a well-formed expression?
(define (expr? x)
  (match x
    [(? integer? i) #t]
    [(? boolean? b) #t]
    [(? char? c) #t]
    [(? string? s) #t]
    [''() #t]
    [`',x (symbol? x)]
    [`(if ,x ,y ,z)
     (and (expr? x)
          (expr? y)
          (expr? z))]
    [`(gensym) #t]    
    [`(,(? prim1?) ,x) (expr? x)]
    [`(,(? prim2?) ,x ,y) (and (expr? x) (expr? y))]
    [(list 'cond `(,xs ,ys) ... `(else ,z))
     (and (andmap expr? xs)
          (andmap expr? ys)
          (expr? z))]
    [(? variable? x) #t]
    [`(let ,x ,y)
     (and (bindings? x)
          (unique? (map first x))
          (expr? y))]
    [`(letrec ,x ,y)
     (and (bindings? x)
          (unique? (map first x))
          (andmap lambda? (map second x))
          (expr? y))]
    [`(apply ,x ,y)
     (and (expr? x)
          (expr? y))]
    [`(λ ,x ,y)
     (and (formals? x)
          (expr? y))]
    [`(,x . ,xs)
     (and (expr? x)
          (andmap expr? xs))]
    [_ #f]))

;; Any -> Boolean
(define (formals? x)
  (match x
    [(list xs ...)
     (and (andmap variable? xs) (unique? xs))]
    [(list-rest xs ... r)
     (and (andmap variable? (cons r xs)) (unique? (cons r xs)))]))

;; Program -> Boolean
;; Is p a closed program?
(define (closed? p)
  (match p
    [(list 'begin `(define (,fs . ,xss) ,es) ... e)
     (and (andmap
           (lambda (xs e)
             (match xs
               [(list xs ...)
                (closed-expr?/env e (append fs xs))]
               [(list* xs ... r)
                (closed-expr?/env e (append fs xs (list r)))]))
           xss es)
          (closed-expr?/env e fs))]
    [e (closed-expr?/env e '())]))

(define (closed-expr?/env e bvs)
  (match e
    [(? integer?) #t]
    [(? boolean?) #t]
    [(? char?) #t]
    [(? string?) #t]
    [(? symbol?) (and (memq e bvs) #t)]    
    [''() #t]
    [`',x #t]
    [`(if . ,es) (andmap (λ (e) (closed-expr?/env e bvs)) es)]
    [`(gensym) #t]
    [`(,(? prim1?) ,e) (closed-expr?/env e bvs)]
    [`(,(? prim2?) ,e0 ,e1)
     (and (closed-expr?/env e0 bvs)
          (closed-expr?/env e1 bvs))]
    [(list 'cond `(,e0s ,e1s) ... `(else ,en))
     (andmap (λ (e) (closed-expr?/env e bvs))
             (append e0s e1s (list en)))]
    [`(let ,(list `(,xs ,e0s) ...) ,e)
     (and (andmap (λ (e) (closed-expr?/env e bvs)) e0s)
          (closed-expr?/env e (append xs bvs)))]
    [`(letrec ,(list `(,xs ,e0s) ...) ,e)
     (and (andmap (λ (e) (closed-expr?/env e (append xs bvs))) e0s)
          (closed-expr?/env e (append xs bvs)))]
    [`(apply ,e0 ,e1)
     (and (closed-expr?/env e0 bvs)
          (closed-expr?/env e1 bvs))]
    [`(λ ,(list xs ...) ,e)
     (closed-expr?/env e (append xs bvs))]
    [`(λ ,(list-rest xs ... r) ,e)
     (closed-expr?/env e (cons r (append xs bvs)))]
    [`(,e . ,es)
     (and (closed-expr?/env e bvs)
          (andmap (lambda (e) (closed-expr?/env e bvs)) es))]))

;; Any -> Boolean
;; Is x a unary primitive?
(define (prim1? x)
  (and (symbol? x)
       (memq x '(add1 sub1 abs - integer->char char->integer
                      car cdr length box? string? cons? empty?
                      box unbox string-length char? integer? boolean? zero?
                      gensym))))

;; Any -> Boolean
;; Is x a binary primitive?
(define (prim2? x)
  (and (symbol? x)
       (memq x '(+ cons string-ref make-string char=?
                   = <= < char=? boolean=? - eq?))))

;; Any -> Boolean
;; Is x a well-formed list of bindings?
(define (bindings? x)
  (match x
    ['() #t]
    [`((,x ,y) . ,z)
     (and (variable? x)
          (expr? y)
          (bindings? z))]
    [_ #f]))

;; Expr -> Boolean
(define (lambda? e)
  (match e
    [`(λ ,x ,e) #t]
    [_ #f]))

;; Any -> Boolean
(define (keyword? x)
  (and (symbol? x)
       (memq x '(cond else if let letrec apply λ quote gensym))))

;; Any -> Boolean
(define (variable? x)
  (and (symbol? x)
       (not (prim1? x))
       (not (prim2? x))
       (not (keyword? x))))

;; Any -> Boolean
(define (imm? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (equal? ''() x)))

(module+ test
  (require rackunit)

  (check-true (expr? 1))
  (check-true (expr? #\c))
  (check-true (expr? #t))
  (check-true (expr? '(add1 1)))
  (check-true (expr? '(add1 #t)))
  (check-true (expr? '(let () 1)))
  (check-true (expr? '(let ((x 1)) 1)))
  (check-true (expr? '(let ((x 1)) x)))

  (check-false (expr? '(let ((x 1) (x 2)) x)))

  (check-false (expr? '(let ((x)) 0)))
  (check-false (expr? '(let 0 0)))
  (check-false (expr? '(let (x) 0)))
  (check-false (expr? '(let (()) 0)))
  (check-false (expr? '(let x 0)))
  (check-false (expr? '(let x)))
  (check-false (expr? '(let ((x 0)))))

  (check-true (expr? '(letrec ((f (λ (x) x))) (f 5))))
  (check-true (expr? '(λ (x) x)))
  (check-true (expr? '(λ x x)))
  (check-true (expr? '((λ x x) (λ x x))))
  (check-true (expr? ''fred))
  (check-true (expr? '((λ x x) 'fred)))
  (check-true (expr? '(apply (λ x x) 'fred)))

  (check-false (expr? '(letrec ((f (add1 5))) (f 5))))
  (check-false (expr? '(λ #f x)))
  (check-false (expr? '(λ (x x) x))))

;; Expr+ -> Expr
(define (desugar e+)
  (match e+
    [`(begin ,@(list `(define (,fs . ,xss) ,es) ...) ,e)
     `(letrec ,(map (λ (f xs e) `(,f (λ ,xs ,(desugar e)))) fs xss es)
        ,(desugar e))]
    [(? variable? x)          x]
    [(? imm? i)               i]
    [(? string? s)            s]
    [`',x                     `',x]
    [`(gensym)                `(gensym)]
    [`(if ,e0 ,e1 ,e2)        `(if ,(desugar e0) ,(desugar e1) ,(desugar e2))]
    [`(- ,e0)                 `(- 0 ,(desugar e0))]
    [`(,(? prim1? p) ,e0)     `(,p ,(desugar e0))]
    [`(,(? prim2? p) ,e0 ,e1) `(,p ,(desugar e0) ,(desugar e1))]
    [`(cond [else ,e0])       (desugar e0)]
    [`(apply ,e0 ,e1)         `(apply ,(desugar e0) ,(desugar e1))]    
    [`(cond [,q0 ,e0] . ,r)
     `(if ,(desugar q0)
          ,(desugar e0)
          ,(desugar `(cond . ,r)))]
    [`(let ,bs ,e0)
     `(let ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (desugar (second b)))) bs)
        ,(desugar e0))]
    [`(λ ,xs ,e0)          `(λ ,xs ,(desugar e0))]
    [`(,e . ,es)           `(,(desugar e) ,@(map desugar es))]))

;; Expr -> LExpr
(define (label-λ e)
  (match e
    [(? variable? x)          x]
    [(? imm? i)               i]
    [(? string? s)            s]
    [`',x                     `',x]
    [`(gensym)                `(gensym)]
    [`(if ,e0 ,e1 ,e2)        `(if ,(label-λ e0) ,(label-λ e1) ,(label-λ e2))]
    [`(apply ,e0 ,e1)         `(apply ,(label-λ e0) ,(label-λ e1))]    
    [`(,(? prim1? p) ,e0)     `(,p ,(label-λ e0))]
    [`(,(? prim2? p) ,e0 ,e1) `(,p ,(label-λ e0) ,(label-λ e1))]
    [`(let ,bs ,e0)
     `(let ,(map (λ (b) (list (first b) (label-λ (second b)))) bs)
        ,(label-λ e0))]
    [`(letrec ,bs ,e0)
     `(letrec ,(map (λ (b) (list (first b) (label-λ (second b)))) bs)
        ,(label-λ e0))]
    [`(λ ,xs ,e0)             `(λ ,xs ',(gensym) ,(label-λ e0))]
    [`(,e . ,es)              `(,(label-λ e) ,@(map label-λ es))]))

;; LExpr -> (Listof LExpr)
;; Extract all the lambda expressions
(define (λs e)
  (match e
    [(? variable? x)        '()]
    [(? imm? i)             '()]
    [(? string? s)          '()]
    [`',x                   '()]
    [`(gensym)              '()]
    [`(,(? prim1?) ,e0)     (λs e0)]
    [`(,(? prim2?) ,e0 ,e1) (append (λs e0) (λs e1))]
    [`(if ,e0 ,e1 ,e2)      (append (λs e0) (λs e1) (λs e2))]
    [`(apply ,e0 ,e1)       (append (λs e0) (λs e1))]
    [`(let ,bs ,e0)         (append (apply append (map (compose λs second) bs)) (λs e0))]
    [`(letrec ,bs ,e0)      (append (apply append (map (compose λs second) bs)) (λs e0))]
    [`(λ ,xs ,l ,e0)        (cons e (λs e0))]
    [`(,e . ,es)            (append (λs e) (apply append (map λs es)))]))

;; LExpr -> (Listof Variable)
(define (fvs e)
  (define (fvs e)
    (match e
      [(? variable? x)        (list x)]
      [(? imm? i)             '()]
      [(? string? s)          '()]
      [`',x                   '()]
      [`(gensym)              '()]
      [`(,(? prim1?) ,e0)     (fvs e0)]
      [`(,(? prim2?) ,e0 ,e1) (append (fvs e0) (fvs e1))]
      [`(if ,e0 ,e1 ,e2)      (append (fvs e0) (fvs e1) (fvs e2))]
      [`(apply ,e0 ,e1)       (append (fvs e0) (fvs e1))]      
      [`(let ,bs ,e0)
       (append (apply append (map (compose fvs second) bs))
               (remq* (map first bs)
                      (fvs e0)))]
      [`(letrec ,bs ,e0)
       (remq* (map first bs)
              (apply append (fvs e0) (map fvs (map second bs))))]
      [`(λ ,(list xs ...) ,l ,e0)
       (remq* xs (fvs e0))]
      [`(λ ,(list-rest xs ... r) ,l ,e)
       (remq* (cons r xs) (fvs e))]
      [`(,e . ,es)
       (append (fvs e) (apply append (map fvs es)))]))
  (remove-duplicates (fvs e)))

(module+ test
  (require rackunit)
  (check-equal? (desugar '(begin (define (f x) x) (f 1)))
                '(letrec ((f (λ (x) x))) (f 1)))
  (check-equal? (desugar '(cond [(empty? 1) 2] [else 3]))
                '(if (empty? 1) 2 3))
  (check-equal? (desugar '(begin (define (f . x) x)
                                 (f 1)))
                '(letrec ((f (λ x x))) (f 1)))
  (check-equal? (desugar '(+ (begin (define (f x) x) (f 5)) 2))
                '(+ (letrec ((f (λ (x) x))) (f 5)) 2))
  (check-equal? (desugar '(λ x x))
                '(λ x x))
  (check-equal? (desugar ''x) ''x)

  (define (lset=? ls1 ls2) (set=? (apply set ls1) (apply set ls2)))
  (check lset=? (fvs 'x) '(x))
  (check lset=? (fvs '(let ((x 1)) x)) '())
  (check lset=? (fvs '(let ((x x)) x)) '(x))
  (check lset=? (fvs '(letrec ((x x)) x)) '())
  (check lset=? (fvs '(λ (x) 'f x)) '())
  (check lset=? (fvs '(apply f x)) '(f x))
  (check lset=? (fvs ''x) '())
  (check lset=? (fvs '(letrec ((f (λ (x) 'f (f x))))
                        (f 1)))
         '())
  (check lset=? (fvs '(letrec ((f (λ (x) 'f (h x)))
                               (h (λ (x) 'h (f x))))
                        (f 1)))
         '()))
