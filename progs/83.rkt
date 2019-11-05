#lang racket
(if (cond
     (#\uEA75 (zero? #f))
     ((if #f (if #t #f #f) #t) (integer->char #t))
     (19 -2)
     (#\뿃 #t)
     (#t #\U00073553)
     ((if #\U0001B9BC (if #f 1 #\ꀘ) (add1 #\𧤃)) #t)
     ((if (cond (else #\U00044B95)) (if #t 4 #t) #\㦶)
      (if (boolean? #\㦉) (zero? #t) -8))
     ((cond (1 (if -2 #t 1)) (else #\堗)) #f)
     ((if #f 3 #t)
      (if #\붠
        #t
        (cond (#t #\U000360AD) (#t #\U001093CF) (#\U0008689F -1) (else 1))))
     (else
      (if (if #f #f #\U0005BD46)
        (cond (-1 #\U000C8608) (#f -4) (else -1))
        (integer? #t))))
  #t
  (zero? -2))
