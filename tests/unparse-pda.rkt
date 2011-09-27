#lang racket
(require "../pda-data.rkt"
         rackunit
         "../unparse-pda.rkt")

(check-equal? (unparse-pda
               (make-pda '(A B $eos)
                         '$eos
                         's1
                         (list (make-state 's6
                                           (list (make-accept '($eos)))
                                           (list))
                               (make-state 's5
                                           (list (make-reduce '() 'r1))
                                           (list))
                               (make-state 's4
                                           (list (make-shift '(B) 's5))
                                           (list))
                               (make-state 's3
                                           (list (make-reduce '() 'r2))
                                           (list))
                               (make-state 's2
                                           (list (make-shift '(A) 's2)
                                                 (make-shift '(B) 's3))
                                           (list (make-goto 'start 's4)))
                               (make-state 's1
                                           (list (make-shift '(A) 's2))
                                           (list (make-goto 'start 's6))))
                         (list (make-rule 'r3 'accept '(v1 #f) 'v1)
                               (make-rule 'r2 'start '(#f #f) '2)
                               (make-rule 'r1 'start '(#f v2 #f) '(+ 2 v2)))))
              '((TOKENS A B $eos)
                (EOS $eos)
                (START s1)
                (STATE s6 (ACCEPT ($eos)))
                (STATE s5 (REDUCE () r1))
                (STATE s4 (SHIFT (B) s5))
                (STATE s3 (REDUCE () r2))
                (STATE s2
                       (SHIFT (A) s2)
                       (SHIFT (B) s3)
                       (GOTO start s4))
                (STATE s1 (SHIFT (A) s2) (GOTO start s6))
                (RULE r3 accept (v1 #f) v1)
                (RULE r2 start  (#f #f) 2)
                (RULE r1 start  (#f v2 #f) (+ 2 v2))))

(check-equal?
 (unparse-pda
  (make-pda '(NUM L-PAREN R-PAREN SEMICOLON
                  TIMES DIVIDE PLUS MINUS *EOF*)
            '*EOF*
            's0
            (list (make-state
                   's4
                   (list
                    (make-shift '(SEMICOLON) 's19)
                    (make-shift '(TIMES) 's11)
                    (make-shift '(DIVIDE) 's12)
                    (make-shift '(PLUS) 's13)
                    (make-shift '(MINUS) 's14)
                    (make-reduce '(*EOF*) 'r3))
                   (list))
                  (make-state 's3 (list (make-reduce '() 'r6)) (list))
                  (make-state
                   's2
                   (list
                    (make-shift '(NUM) 's5)
                    (make-shift '(L-PAREN) 's6)
                    (make-shift '(*ERROR*) 's7)
                    (make-reduce '(*EOF*) 'r2))
                   (list (make-goto 'statement 's3) (make-goto 'exp 's4)))
                  (make-state 's1 (list (make-accept '(*EOF*))) (list))
                  (make-state
                   's0
                   (list (make-reduce '() 'r5))
                   (list (make-goto 'program 's1) (make-goto 's-list 's2))))
            (list (make-rule 'r14 'exp '(#f exp #f) 'exp)
                  (make-rule 'r13 'exp '(expA #f exp) '(quotient expA exp))
                  (make-rule 'r9 'exp '(NUM) 'NUM)
                  (make-rule 'r5 's-list '() ''())
                  (make-rule 'r4 'program '(s-list #f)
                             '(cons (if #f #f) s-list)))))
 '((TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
   (EOS *EOF*)
   (START s0)
   (RULE r14 exp (#f exp #f) exp)
   (RULE r13 exp (expA #f exp) (quotient expA exp))
   (RULE r9 exp (NUM) NUM)
   (RULE r5 s-list () '())
   (RULE r4 program (s-list #f) (cons (if #f #f) s-list))
   (STATE
    s4
    (SHIFT (SEMICOLON) s19)
    (SHIFT (TIMES) s11)
    (SHIFT (DIVIDE) s12)
    (SHIFT (PLUS) s13)
    (SHIFT (MINUS) s14)
    (REDUCE (*EOF*) r3))
   (STATE s3 (REDUCE () r6))
   (STATE
    s2
    (SHIFT (NUM) s5)
    (SHIFT (L-PAREN) s6)
    (SHIFT (*ERROR*) s7)
    (REDUCE (*EOF*) r2)
    (GOTO statement s3)
    (GOTO exp s4))
   (STATE s1 (ACCEPT (*EOF*)))
   (STATE
    s0
    (REDUCE () r5)
    (GOTO program s1)
    (GOTO s-list s2))))
