#lang racket
(require "../pda-data.rkt"
         rackunit
         "test-util.rkt"
         "../parse-pda.rkt")

(define-test-suite parse-pda-tests
  (test-case
   "typed pdas"
   (check-syntax-equal?
    (parse-pda #'(parse-pda
                  (TOKENS A B $eos)
                  (EOS $eos)
                  (START s1)
                  (STATE s1 (())
                         (SHIFT (A) s2)
                         (GOTO start s6))
                  (STATE s2 ((A s1)
                             (A s2))
                         (SHIFT (A) s2)
                         (SHIFT (B) s3)
                         (GOTO start s4))
                  (STATE s3 ((B s2 A s1)
                             (B s2 A s2))
                         (REDUCE () r2))
                  (STATE s4 ((start s4 A s1)
                             (start s4 A s2))
                         (SHIFT (B) s5))
                  (STATE s5 ((B s5 start s4 A s1)
                             (B s5 start s4 A s2))
                         (REDUCE () r1))
                  (STATE s6 ((start s1))
                         (ACCEPT ($eos)))
                  (RULE r1 ((B s5 start s4 A s2))
                        start  (#f v2 #f) (+ 2 v2))
                  (RULE r2 ((B s2 A s2))
                        start  (#f #f) 2)))
    (make-pda (list #'A #'B #'$eos)
              #'$eos
              #'s1
              (list (make-state #'s1 '(())
                                (list (make-shift (list #'A) #'s2))
                                (list)
                                (list (make-goto 'start #'s6)))
                    (make-state #'s2 '((A s1) (A s2))
                                (list (make-shift (list #'A) #'s2)
                                      (make-shift (list #'B) #'s3))
                                (list)
                                (list (make-goto 'start #'s4)))
                    (make-state #'s3 '((B s2 A s1)
                                       (B s2 A s2))
                                (list (make-reduce '() #'r2))
                                (list)
                                (list))
                    (make-state #'s4 '((start s4 A s1)
                                       (start s4 A s2))
                                (list (make-shift (list #'B) #'s5))
                                (list)
                                (list))
                    (make-state #'s5 '((B s5 start s4 A s1)
                                       (B s5 start s4 A s2))
                                (list (make-reduce '() #'r1))
                                (list)
                                (list))
                    (make-state #'s6 '((start s1))
                                (list)
                                (list (make-accept (list #'$eos)))
                                (list)))
              (list (make-rule #'r1 '((B s5 start s4 A s2))
                               'start (list #f #'v2 #f) #'(+ 2 v2))
                    (make-rule #'r2 '((B s2 A s2))
                               'start '(#f #f) #'2))))

   (check-syntax-equal?
    (parse-pda
     #'(parse-pda
        (TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
        (EOS *EOF*)
        (START s0)
        (RULE r4 (()) program (s-list #f) (cons (if #f #f) s-list))
        (RULE r5 (()) s-list () '())
        (RULE r9 (()) exp (NUM) NUM)
        (RULE r13 (()) exp (expA #f exp) (quotient expA exp))
        (RULE r14 (()) exp (#f exp #f) exp)
        (STATE
         s0 (())
         (COMMENT s-list "=>" "." s-list statement)
         (COMMENT s-list "=>" ".")
         (COMMENT program "=>" "." s-list *ERROR*)
         (COMMENT program "=>" "." s-list exp)
         (COMMENT program "=>" "." s-list)
         (COMMENT *start "=>" "." program *EOF*)
         (REDUCE () r5)
         (GOTO program s1)
         (GOTO s-list s2))
        (STATE s1 (()) (COMMENT *start "=>" program "." *EOF*) (ACCEPT (*EOF*)))
        (STATE
         s2 (())
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
        (STATE s3  (())
               (COMMENT s-list "=>" s-list statement ".") (REDUCE () r6))
        (STATE
         s4 (())
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
         (REDUCE (*EOF*) r3))))
    (make-pda (list #'NUM
                    #'L-PAREN
                    #'R-PAREN
                    #'SEMICOLON
                    #'TIMES
                    #'DIVIDE
                    #'PLUS
                    #'MINUS
                    #'*EOF*)
              #'*EOF*
              #'s0
              (list (make-state
                     #'s0
                     '(())
                     (list (make-reduce '() #'r5))
                     (list)
                     (list (make-goto 'program #'s1) (make-goto 's-list #'s2)))
                    (make-state #'s1 '(())
                                (list)
                                (list (make-accept (list #'*EOF*)))
                                (list))
                    (make-state
                     #'s2
                     '(())
                     (list
                      (make-shift (list #'NUM) #'s5)
                      (make-shift (list #'L-PAREN) #'s6)
                      (make-shift (list #'*ERROR*) #'s7))
                     (list (make-reduce (list #'*EOF*) #'r2))
                     (list (make-goto 'statement #'s3) (make-goto 'exp #'s4)))
                    (make-state #'s3 '(())
                                (list (make-reduce '() #'r6))
                                (list)
                                (list))
                    (make-state
                     #'s4
                     '(())
                     (list
                      (make-shift (list #'SEMICOLON) #'s19)
                      (make-shift (list #'TIMES) #'s11)
                      (make-shift (list #'DIVIDE) #'s12)
                      (make-shift (list #'PLUS) #'s13)
                      (make-shift (list #'MINUS) #'s14))
                     (list (make-reduce (list #'*EOF*) #'r3))
                     (list)))
              (list (make-rule #'r4 '(()) 'program (list #'s-list #f)
                               #'(cons (if #f #f) s-list))
                    (make-rule #'r5 '(()) 's-list '()
                               #''())
                    (make-rule #'r9 '(()) 'exp (list #'NUM)
                               #'NUM)
                    (make-rule #'r13 '(()) 'exp (list #'expA #f #'exp)
                               #'(quotient expA exp))
                    (make-rule #'r14 '(()) 'exp (list #f #'exp #f)
                               #'exp)))))
  (test-case
   "untyped pds"
   (check-syntax-equal?
    (parse-pda #'(parse-pda
                  (TOKENS A B $eos)
                  (EOS $eos)
                  (START s1)
                  (STATE s1
                         (SHIFT (A) s2)
                         (GOTO start s6))
                  (STATE s2
                         (SHIFT (A) s2)
                         (SHIFT (B) s3)
                         (GOTO start s4))
                  (STATE s3
                         (REDUCE () r2))
                  (STATE s4
                         (SHIFT (B) s5))
                  (STATE s5
                         (REDUCE () r1))
                  (STATE s6
                         (ACCEPT ($eos)))
                  (RULE r1
                        start  (#f v2 #f) (+ 2 v2))
                  (RULE r2
                        start  (#f #f) 2)))
    (make-pda (list #'A #'B #'$eos)
              #'$eos
              #'s1
              (list (make-state #'s1 '(())
                                (list (make-shift (list #'A) #'s2))
                                (list)
                                (list (make-goto 'start #'s6)))
                    (make-state #'s2 '((A s2) (A s1))
                                (list (make-shift (list #'A) #'s2)
                                      (make-shift (list #'B) #'s3))
                                (list)
                                (list (make-goto 'start #'s4)))
                    (make-state #'s3 '((B s2 A s2) (B s2 A s1))
                                (list (make-reduce (list) #'r2))
                                (list)
                                (list))
                    (make-state #'s4 '((start s2 A s2) (start s2 A s1))
                                (list (make-shift (list #'B) #'s5))
                                (list)
                                (list))
                    (make-state #'s5 '((B s4 start s2 A s2) (B s4 start s2 A s1))
                                (list (make-reduce (list) #'r1))
                                (list)
                                (list))
                    (make-state #'s6 '((start s1))
                                (list)
                                (list (make-accept (list #'$eos)))
                                (list)))
              (list (make-rule #'r1 '((B s4 start s2 A s2) (B s4 start s2 A s1))
                               'start
                               (list #f #'v2 #f)
                               #'(+ 2 v2))
                    (make-rule #'r2 '((B s2 A s2) (B s2 A s1))
                               'start
                               (list #f #f)
                               #'2))))))

(require rackunit/text-ui)
(run-tests parse-pda-tests)
