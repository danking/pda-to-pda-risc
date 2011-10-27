(require "macro-glue.rkt")

(define ab-matching
  (pda (lambda (x) x) car cdr empty?
       ((TOKENS A B $eos)
        (EOS $eos)
        (START s1)
        (STATE s1 : (())
               (SHIFT (A) s2)
               (GOTO start s6))
        (STATE s2 : ((A s1)
                     (A s2))
               (SHIFT (A) s2)
               (SHIFT (B) s3)
               (GOTO start s4))
        (STATE s3 : ((B s2 A s1)
                     (B s2 A s2))
               (REDUCE () r2))
        (STATE s4 : ((start s4 A s1)
                     (start s4 A s2))
               (SHIFT (B) s5))
        (STATE s5 : ((B s5 start s4 A s1)
                     (B s5 start s4 A s2))
               (REDUCE () r1))
        (STATE s6 : ((start s1))
               (ACCEPT ($eos)))
        (RULE r1 : ((B s5 start s4 A s2))
              start  (#f v2 #f) (+ 2 v2))
        (RULE r2 : ((B s2 A s2))
              start  (#f #f) 2))))

(ab-matching '())
;; error message
(ab-matching '(A
               B))
;; '((2))
(ab-matching '(A A A A
               B B B B))
;; '((8))

(define calculator
  (pda (lambda (t)
         (if (number? t)
             'NUM
             (case t
               ((#\() 'L-PAREN)
               ((#\)) 'R-PAREN)
               ((#\;) 'SEMICOLON)
               ((+) 'PLUS)
               ((-) 'MINUS)
               ((*) 'TIMES)
               ((/) 'DIVIDE)
               ((*EOF*) t)
               (else 'LEXER-ERROR))))
       car
       cdr
       empty?
       ((TOKENS NUM L-PAREN R-PAREN SEMICOLON
                TIMES DIVIDE PLUS MINUS *EOF*)
        (EOS *EOF*)
        (START s0)
        (RULE r2 program (s-list) s-list)
        (RULE
         r3
         program
         (s-list exp)
         (begin (display exp) (newline) (cons exp s-list)))
        (RULE r4 program (s-list *ERROR*)
              (cons (if #f #f (void)) s-list))
        (RULE r5 s-list () '())
        (RULE r6 s-list (s-list statement)
              (cons statement s-list))
        (RULE r7 statement (exp SEMICOLON)
              (begin (display exp) (newline) exp))
        (RULE r8 statement (*ERROR* SEMICOLON)
              (if #f #f (void)))
        (RULE r9 exp (NUM) NUM)
        (RULE r10 exp (exp-1 PLUS exp-2) (+ exp-1 exp-2))
        (RULE r11 exp (exp-1 MINUS exp-2) (- exp-1 exp-2))
        (RULE r12 exp (expA TIMES expB) (* expA expB))
        (RULE r13 exp (expA DIVIDE exp) (quotient expA exp))
        (RULE r14 exp (L-PAREN exp R-PAREN) exp)
        (STATE
         s0
         (COMMENT s-list "=>" "." s-list statement)
         (COMMENT s-list "=>" ".")
         (COMMENT program "=>" "." s-list *ERROR*)
         (COMMENT program "=>" "." s-list exp)
         (COMMENT program "=>" "." s-list)
         (COMMENT *start "=>" "." program *EOF*)
         (REDUCE () r5)
         (GOTO program s1)
         (GOTO s-list s2))
        (STATE s1 (COMMENT *start "=>" program "." *EOF*)
               (ACCEPT (*EOF*)))
        (STATE
         s2
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (COMMENT statement "=>" "." *ERROR* SEMICOLON)
         (COMMENT statement "=>" "." exp SEMICOLON)
         (COMMENT s-list "=>" s-list "." statement)
         (COMMENT program "=>" s-list "." *ERROR*)
         (COMMENT program "=>" s-list "." exp)
         (COMMENT program "=>" s-list ".")
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (SHIFT (*ERROR*) s7)
         (REDUCE (*EOF*) r2)
         (GOTO statement s3)
         (GOTO exp s4))
        (STATE s3
               (COMMENT s-list "=>" s-list statement ".")
               (REDUCE () r6))
        (STATE
         s4
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp "." PLUS exp)
         (COMMENT statement "=>" exp "." SEMICOLON)
         (COMMENT program "=>" s-list exp ".")
         (SHIFT (SEMICOLON) s19)
         (SHIFT (TIMES) s11)
         (SHIFT (DIVIDE) s12)
         (SHIFT (PLUS) s13)
         (SHIFT (MINUS) s14)
         (REDUCE (*EOF*) r3))
        (STATE s5 (COMMENT exp "=>" NUM ".") (REDUCE () r9))
        (STATE
         s6
         (COMMENT exp "=>" L-PAREN "." exp R-PAREN)
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (GOTO exp s9))
        (STATE
         s7
         (COMMENT statement "=>" *ERROR* "." SEMICOLON)
         (COMMENT program "=>" s-list *ERROR* ".")
         (SHIFT (SEMICOLON) s8)
         (REDUCE (*EOF*) r4))
        (STATE s8 (COMMENT statement "=>" *ERROR* SEMICOLON ".")
               (REDUCE () r8))
        (STATE
         s9
         (COMMENT exp "=>" L-PAREN exp "." R-PAREN)
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp "." PLUS exp)
         (SHIFT (R-PAREN) s10)
         (SHIFT (TIMES) s11)
         (SHIFT (DIVIDE) s12)
         (SHIFT (PLUS) s13)
         (SHIFT (MINUS) s14))
        (STATE s10 (COMMENT exp "=>" L-PAREN exp R-PAREN ".")
               (REDUCE () r14))
        (STATE
         s11
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" exp TIMES "." exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (GOTO exp s18))
        (STATE
         s12
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" exp DIVIDE "." exp)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (GOTO exp s17))
        (STATE
         s13
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" exp PLUS "." exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (GOTO exp s16))
        (STATE
         s14
         (COMMENT exp "=>" "." L-PAREN exp R-PAREN)
         (COMMENT exp "=>" "." exp DIVIDE exp)
         (COMMENT exp "=>" "." exp TIMES exp)
         (COMMENT exp "=>" exp MINUS "." exp)
         (COMMENT exp "=>" "." exp MINUS exp)
         (COMMENT exp "=>" "." exp PLUS exp)
         (COMMENT exp "=>" "." NUM)
         (SHIFT (NUM) s5)
         (SHIFT (L-PAREN) s6)
         (GOTO exp s15))
        (STATE
         s15
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp MINUS exp ".")
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp "." PLUS exp)
         (REDUCE (R-PAREN) r11)
         (REDUCE (SEMICOLON) r11)
         (SHIFT (TIMES) s11)
         (SHIFT (DIVIDE) s12)
         (REDUCE (PLUS) r11)
         (REDUCE (MINUS) r11)
         (REDUCE (*EOF*) r11))
        (STATE
         s16
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp PLUS exp ".")
         (COMMENT exp "=>" exp "." PLUS exp)
         (REDUCE (R-PAREN) r10)
         (REDUCE (SEMICOLON) r10)
         (SHIFT (TIMES) s11)
         (SHIFT (DIVIDE) s12)
         (REDUCE (PLUS) r10)
         (REDUCE (MINUS) r10)
         (REDUCE (*EOF*) r10))
        (STATE
         s17
         (COMMENT exp "=>" exp DIVIDE exp ".")
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp "." PLUS exp)
         (REDUCE (R-PAREN) r13)
         (REDUCE (SEMICOLON) r13)
         (REDUCE (TIMES) r13)
         (REDUCE (DIVIDE) r13)
         (REDUCE (PLUS) r13)
         (REDUCE (MINUS) r13)
         (REDUCE (*EOF*) r13))
        (STATE
         s18
         (COMMENT exp "=>" exp "." DIVIDE exp)
         (COMMENT exp "=>" exp TIMES exp ".")
         (COMMENT exp "=>" exp "." TIMES exp)
         (COMMENT exp "=>" exp "." MINUS exp)
         (COMMENT exp "=>" exp "." PLUS exp)
         (REDUCE (R-PAREN) r12)
         (REDUCE (SEMICOLON) r12)
         (REDUCE (TIMES) r12)
         (REDUCE (DIVIDE) r12)
         (REDUCE (PLUS) r12)
         (REDUCE (MINUS) r12)
         (REDUCE (*EOF*) r12))
        (STATE s19 (COMMENT statement "=>" exp SEMICOLON ".")
               (REDUCE () r7)))))


(calculator '(100))
;; 100
;; '((100))
(calculator '(#\( 2 + 7 #\) * #\( #\( 11 - 1 #\) #\) #\; 2 * 21))
;; 90
;; 42
;; '((42 90))
(calculator '(3 + 5 - 2 * 3))
;; 2
;; '((2))
(calculator '(#\( 2 + 7 #\) * #\( #\( 11 - 1 #\) #\) #\; + 5 5 #\; 1234567890))
;; 90
;; big error message

;; Sadism:
(define ab-matching
           (pda (lambda (x) x) car cdr empty?
                ((TOKENS A B $eos)
                 (EOS $eos)
                 (START cdr)
                 (STATE cdr
                        (SHIFT (A) car)
                        (GOTO start regs))
                 (STATE car
                        (SHIFT (A) car)
                        (SHIFT (B) empty?)
                        (GOTO start lambda))
                 (STATE empty?
                        (REDUCE () dict-ref))
                 (STATE lambda
                        (SHIFT (B) s5))
                 (STATE s5
                        (REDUCE () r1))
                 (STATE regs
                        (ACCEPT ($eos)))
                 (RULE r1
                       start  (#f v2 #f) (+ 2 v2))
                 (RULE dict-ref
                       start  (#f #f) 2))))

(require "parse-pda.rkt")

(parse-pda #'(parse-pda (TOKENS A B $eos)
                        (EOS $eos)
                        (START s1)
                        (STATE s1 : (())
                               (SHIFT (A) s2)
                               (GOTO start s6))
                        (STATE s2 : ((A s1)
                                     (A s2))
                               (SHIFT (A) s2)
                               (SHIFT (B) s3)
                               (GOTO start s4))
                        (STATE s3 : ((B s2 A s1)
                                     (B s2 A s2))
                               (REDUCE () r2))
                        (STATE s4 : ((start s4 A s1)
                                     (start s4 A s2))
                               (SHIFT (B) s5))
                        (STATE s5 : ((B s5 start s4 A s1)
                                     (B s5 start s4 A s2))
                               (REDUCE () r1))
                        (STATE s6 : ((start s1))
                               (ACCEPT ($eos)))
                        (RULE r1 : ((B s5 start s4 A s2))
                              start  (#f v2 #f) (+ 2 v2))
                        (RULE r2 : ((B s2 A s2))
                              start  (#f #f) 2)))
