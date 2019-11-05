#lang racket
(add1
 (cond
  ((integer->char
    (char?
     (if (cond
          (-4 #t)
          (#\熩 (cond (#f -1) ((if #t 2 -1) #f) (else 1)))
          ((abs (cond (else #\ꓓ))) (boolean? (if 16 #f 0)))
          (1 #f)
          (else
           (cond
            ((add1 #\U000E9483)
             (cond (#f #t) (#t #\U000B8F86) (#t #\U000A5BED) (else #t)))
            ((if #f #\u4DB7 #t) (abs #f))
            ((char->integer #t) 1)
            (#f #\U0004D380)
            (#\U00043F26 #t)
            (else #t))))
       #f
       #f)))
   #t)
  ((if #\থ -3 (cond ((if (integer? #\U0006538F) #f #f) #f) (else #t)))
   (if 17
     (cond
      (#t #t)
      (#\𥱢 #\u08D6)
      (#f #\ꉇ)
      (#f 3)
      (#\쒙 #f)
      (#\ⶼ 0)
      (4 -2)
      (4 -1)
      (#f 3)
      (#\U000A10A1 (sub1 #f))
      (#\踗 #\ꨪ)
      (#f #\U0010836B)
      (#\U0006C8F5
       (if (cond
            ((cond
              (else
               (cond
                (-8 -1)
                (#t -4)
                (#\𤙉 4)
                (#t #\U000CF888)
                (#t #t)
                (else 8))))
             #t)
            (-1 #\U000D80E9)
            (else -2))
         (if #t (char? #t) #f)
         #f))
      (else #t))
     #t))
  ((cond
    ((char->integer #\U0005CFF4)
     (cond
      (#t #f)
      (#t #\U000C59C9)
      (2 #\U000410DE)
      (-1 2)
      (#\U00037D7E #\Ⴜ)
      (#t #f)
      (#t 2)
      (else #t)))
    (else #\U00068EC5))
   #t)
  ((cond (else (if #\욕 #f 4))) #\U00058715)
  (#\씔 (if #\U000E6BAD (let ((g1 #f)) g1) -2))
  (else (if (if 5 #\竧 #\ᤈ) #f #t))))
