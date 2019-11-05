#lang racket
(char->integer
 (cond
  ((integer->char -1) 2)
  ((- (if (char->integer -2) #t -2)) #f)
  ((cond (0 #f) (-2 #f) (else #\U00090875)) #t)
  (#t
   (cond
    ((if (if #f #t #\ជ) (if #\U00066753 #t #f) (if #\턂 #\ꊤ -1)) #\U000D874E)
    (else (if #f (if #\ᮀ #\띒 #f) (if -16 #t #f)))))
  (-4 4)
  (5 #\䪭)
  (else #\큨)))
