#lang racket
(if (let ((w0 #t)) #t)
  (abs
   (if -19
     (cond
      ((cond (else 2)) #t)
      ((if #\U00081A5F #\U000A6656 #t)
       (cond
        (#t 0)
        (#t 1)
        (#t 10)
        (-3 2)
        (#t #\U0001A5D2)
        (#t #\▖)
        (2 #f)
        (#f #t)
        (else #\U0005C555)))
      (#\U000BEEAE (cond (else -9)))
      (#\U0008EB1F (if #t 2 #t))
      ((if 0 #t #f) #\牖)
      ((if #t #f #t) -2)
      (#\U000B5C28 #\U000E881D)
      (else #t))
     #f))
  (- #f))
