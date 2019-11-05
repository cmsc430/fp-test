#lang racket
(if (char=? (string-ref "abc" 2) (integer->char (char->integer #\c)))
    (box "Fred")
    (unbox (box "Wilma")))
