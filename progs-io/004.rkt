#lang racket
(begin

  (define (divide n m)
    (if (< n m)
        0
        (add1 (divide (- n m) m))))

  (define (msd n)
    (if (< n 10)
        n
        (msd (divide n 10))))

  (define (digit-length n)
    (if (< n 10)
        1
        (add1 (digit-length (divide n 10)))))

  (define (shift-right-base10 n)
    (- n
       (mult (msd n) (expt 10 (sub1 (digit-length n))))))

  (define (expt n m)
    (match m
      [0 1]
      [1 n]
      [m (mult n (expt n (sub1 m)))]))

  (define (mult n m)
    (if (= n 0)
        0
        (if (= n 1)
            m
            (+ m (mult (sub1 n) m)))))

  (define (number->string n)
    (if (< n 0)
        (string-append "-" (number->string (- 0 n)))
        (letrec ((loop
                  (λ (n)
                    (if (< n 10)
                        (number->digit n)
                        (string-append (number->digit (msd n))
                                       (loop (shift-right-base10 n)))))))
          (loop n))))

  (define (number->digit n)
    (match n
      [0 "0"]
      [1 "1"]
      [2 "2"]
      [3 "3"]
      [4 "4"]
      [5 "5"]
      [6 "6"]
      [7 "7"]
      [8 "8"]
      [9 "9"]))
  
  (define (findf p xs)
    (match xs
      ['() #f]
      [(cons x xs)
       (if (p x)
           x
           (findf p xs))]))

  ;---------------------------------------------------  
  (define (system-type s)
    (match s
      ['os 'unix]))
  
  (define (string-append* ss)
    (match ss
      ['() ""]
      [(cons s ss)
       (string-append s
                      (string-append* ss))]))
  
  (define (opcode1? x)
    (memq x '(sete)))

  (define (opcode2? x)
    (memq x '(mov add sub cmp and cmovl xor or sal sar lea)))

  (define (asm->string a)
    (foldr (λ (i s) (string-append (instr->string i) s)) "" a))

  (define (instr->string i)
    (match i
      [(list (? opcode2? o) a1 a2)
       (string-append* (list "\t"
                             (symbol->string o) " "
                             (arg->string a1) ", "
                             (arg->string a2) "\n"))]))


  (define (arg->string a)
    (match a
      [(? reg?) (reg->string a)]
      [(list 'offset r)
       (string-append* (list "[" (arg->string r) "]"))]
      [(list 'offset r i)
       (string-append* (list "[" (arg->string r) " + " (number->string (arithmetic-shift i 3)) "]"))]
      [(? integer?) (number->string a)]
      [_ (label->string a)]))

  (define (reg? x)
    (memq x '(rax rbx rcx rdx rsp rdi rip rbp rsi r8 r9 r10 r11 r12 r13 r14 r15 al cl)))

  (define (reg->string r)
    (symbol->string r))

  (define (label->string s)
    (match (system-type 'os)
      ['macosx
       (string-append "_" (symbol->string s))]
      [_ (symbol->string s)]))

  ;; Asm -> Void
  (define (asm-display a)
    ;; entry point will be first label
    (let ((g (findf (λ (x) (symbol? x)) a)))
      (display
       (string-append* (list "\tglobal " (label->string g) "\n"
                             "\tdefault rel\n"
                             "\textern " (label->string 'error) "\n"
                             "\textern " (label->string 'read_char) "\n"
                             "\textern " (label->string 'write_char) "\n"
                             "\tsection .text\n"
                             (asm->string a))))))

  100)
