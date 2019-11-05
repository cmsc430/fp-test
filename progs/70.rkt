#lang racket
(let ((g2
       (cond
        (8 (integer? (add1 #t)))
        ((add1 (if #\U000C6194 #\狄 -2))
         (if (if #t -6 #f) (if #f #\U000A976D #\曗) (char->integer #f)))
        ((let ((m1 (cond (#\U00097977 #t) (#\碪 #f) (else 1)))) 3) #f)
        (#f #\U000A53EF)
        (else #\U000856D7))))
  #\U0009B7A9)
