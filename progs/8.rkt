#lang racket
(cond
 ((cond (else (let ((d2 #f)) (let ((h1 #t)) #\䯌)))) #t)
 (6 (integer->char 5))
 (1 (char? (cond (else (char->integer #t)))))
 (#\U000401F0
  (if #f
    (if (integer?
         (cond
          (#f (abs (cond (#\崵 -2) (#t #t) (else #t))))
          (#t (boolean? #f))
          ((if #\威
             (if #t
               (cond (-3 #\U00108B90) (#\ᐦ #f) (else #t))
               (integer->char #f))
             (add1 (cond (2 #f) (#\U000E44EE #\⅌) (else #\㝄))))
           #\U000A8A11)
          (#f -1)
          (2 #f)
          ((char->integer (boolean? #f))
           (if (if (add1 #\랞) #t #t)
             (cond (#t 5) (1 #t) (-2 (if #t -3 #t)) (else #f))
             (sub1 (cond (-16 #f) (#t #f) (#\U000C838C #t) (else #\鱱)))))
          (#t #\愠)
          ((if #t #f #t) #f)
          (else #\闟)))
      (if #f
        #\U00100215
        (if (if #t
              (if (if #t #f -1) #\U0005E5F0 #f)
              (if (if #\穲 #t #t) (boolean? #\蹖) -1))
          #\⠛
          (if (integer->char 8) -2 #f)))
      #f)
    (zero? #t)))
 (#t 0)
 (-9 #t)
 (else #\䲸))
