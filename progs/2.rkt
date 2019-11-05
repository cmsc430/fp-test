#lang racket
(integer->char
 (cond
  (#\U0002E082
   (cond (#f #f) (#f (sub1 #\𖢞)) ((add1 16) (integer? -2)) (else #t)))
  (else (sub1 (char? #\贿)))))
