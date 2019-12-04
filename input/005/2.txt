#lang racket

;; Lex simple programs

(begin

  (define (read-chars)
    (let ((c (read-char)))
      (if (eof-object? c)
          '()
          (cons c (read-chars)))))
  
  (define (parse-prog loc)
    (match loc
      [(cons #\# (cons #\l (cons #\a (cons #\n (cons #\g (cons #\space (cons #\r (cons #\a (cons #\c (cons #\k (cons #\e (cons #\t (cons #\newline loc)))))))))))))
       (parse-token* loc)]))
  
  (define (parse-token* loc)
    (match loc
      ['() '()]
      [(cons (? whitespace?) loc)
       (parse-token* loc)]
      [(cons #\; loc)
       (match (parse-comment loc)
         [loc (parse-token* loc)])]
      [_ (match (parse-token loc)
           [(cons loc t)
            (cons t (parse-token* loc))])]))

  (define (parse-comment loc)
    (match loc
      ['() '()]
      [(cons #\newline loc) loc]
      [(cons _ loc) (parse-comment loc)]))
  

  (define (whitespace? c)
    (memq c '(#\space #\tab #\newline #\return)))

  ; (Listof Char) -> (Pairof (Listof Char) Token)
  (define (parse-token loc)
    ; <token> -> <identifier> | <boolean> | <character> | <string>
    ;          | ( | [ | ) | ] | ' | .
    (cond
      [(first-identifier? loc)
       (parse-identifier loc)]
      [(first-boolean? loc)
       (parse-boolean loc)]
      [(first-number? loc)
       (parse-number loc)]
      [(first-character? loc)
       (parse-character loc)]
      [(first-string? loc)
       (parse-string loc)]
      [else
       (match loc
         [(cons #\( loc)
          (cons loc 'lparen)]
         [(cons #\[ loc)
          (cons loc 'lbracket)]
         [(cons #\) loc)
          (cons loc 'rparen)]
         [(cons #\] loc)
          (cons loc 'rbracket)]
         [(cons #\' loc)
          (cons loc 'quote)]
         [(cons #\. loc)
          (cons loc 'dot)])]))

  (define (first-identifier? loc)
    (match loc
      [(cons (? letter?) _) #t]
      [(cons (? special-initial?) _) #t]
      [(cons #\+ _) #t]
      [(cons #\- _) #t]
      [(cons #\. (cons #\. (cons #\. _))) #t]
      [_ #f]))

  (define (first-boolean? loc)
    (match loc
      [(cons #\# (cons #\t _)) #t]
      [(cons #\# (cons #\f _)) #t]
      [_ #f]))

  (define (first-number? loc)
    (match loc
      [(cons (? digit?) _) #t]
      [_ #f]))

  (define (first-character? loc)
    (match loc
      [(cons #\# (cons #\\ _)) #t]
      [_ #f]))

  (define (first-string? loc)
    (match loc
      [(cons #\" _) #t]
      [_ #f]))

  (define (parse-identifier loc)
    (cond [(first-initial? loc)
           (match (parse-initial loc)
             [(cons loc init)
              (match (parse-subsequent* loc)
                [(cons loc subs)
                 (cons loc (cons 'id (list->string (cons init subs))))])])]
          [else
           (parse-peculiar-identifier loc)]))

  (define (parse-peculiar-identifier loc)
    (match loc
      [(cons #\+ loc) (cons loc (cons 'id "+"))]
      [(cons #\- loc) (cons loc (cons 'id "-"))]
      [(cons #\. (cons #\. (cons #\. loc)))
       (cons loc (cons 'id "..."))]))

  (define (parse-boolean loc)
    (match loc
      [(cons #\# (cons #\t loc)) (cons loc #t)]
      [(cons #\# (cons #\f loc)) (cons loc #f)]))

  (define (parse-number loc)
    'fixme)

  (define (parse-character loc)
    (match loc
      [(cons #\# (cons #\\ (cons #\t (cons #\a (cons #\b loc)))))
       (cons loc #\tab)]
      [(cons  #\# (cons #\\ (cons #\n (cons #\e (cons #\w (cons #\l (cons #\i (cons #\n (cons #\e loc)))))))))
       (cons loc #\newline)]
      [(cons  #\# (cons #\\ (cons #\r (cons #\e (cons #\t (cons #\u (cons #\r (cons #\n loc))))))))
       (cons loc #\return)]
      [(cons #\# (cons #\\ (cons c loc)))
       (cons loc c)]
      ))

  (define (parse-string loc)
    (match loc
      [(cons #\" loc)
       (match (parse-string-element* loc)
         [(cons loc cs)
          (match loc
            [(cons #\" loc)
             (cons loc (list->string cs))])])]))

  (define (parse-string-element* loc)
    (match loc
      [(cons #\" _) (cons loc '())]
      [(cons c loc)
       (match (parse-string-element* loc)
         [(cons loc cs)
          (cons loc (cons c cs))])]))

  (define (special-initial? c)
    (memq c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~)))

  (define (letter? c)
    (memq c (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

  (define (first-initial? loc)
    (match loc
      [(cons (? letter?) _) #t]
      [(cons (? special-initial?) _) #t]
      [_ #f]))

  (define (parse-initial loc)
    (match loc
      [(cons (? letter? l) loc) (cons loc l)]
      [(cons (? special-initial? s) loc) (cons loc s)]))

  (define (parse-subsequent* loc)
    (match loc
      [(? first-subsequent?)
       (match (parse-subsequent loc)
         [(cons loc s)
          (match (parse-subsequent* loc)          
            [(cons loc ss)
             (cons loc (cons s ss))])])]
      [_ (cons loc '())]))

  (define (first-subsequent? loc)
    (or (first-initial? loc)
        (first-digit? loc)
        (first-special-subsequent? loc)))

  (define (parse-subsequent loc)
    (cond
      [(first-initial? loc) (parse-initial loc)]
      [(first-digit? loc) (parse-digit loc)]
      [(first-special-subsequent? loc) (parse-special-subsequent loc)]
      [else (add1 #f)]))

  (define (first-digit? loc)
    (match loc
      [(cons (? digit?) _) #t]
      [_ #f]))

  (define (first-special-subsequent? loc)
    (match loc
      [(cons (? special-subsequent?) _) #t]
      [_ #f]))

  (define (special-subsequent? c)
    (memq c '(#\+ #\- #\. #\@ #\#)))

  (define (parse-digit loc)
    (match loc
      [(cons (? digit? d) loc)
       (cons loc d)]))

  (define (parse-special-subsequent loc)
    (match loc
      [(cons (? special-subsequent? s) loc)
       (cons loc s)]))

  (define (digit? c)
    (memq c (string->list "0123456789")))
  
  (parse-prog (read-chars)))
