#lang racket
(require "../pda-data.rkt"
         rackunit
         "../parse-pda.rkt")

(check-equal? (parse-pda '((TOKENS A B $eos)
                           (EOS $eos)
                           (START s1)
                           (STATE s1 : (())
                                  (SHIFT (A) s2)
                                  (GOTO start s6))
                           (STATE s2 : (s : ((A s1 ())
                                             (A s2 s)))
                                  (SHIFT (A) s2)
                                  (SHIFT (B) s3)
                                  (GOTO start s4))
                           (STATE s3 : (s : ((B s2 A s1 ())
                                             (B s2 A s2 s)))
                                  (REDUCE () r2))
                           (STATE s4 : (s : ((start s4 A s1 ())
                                             (start s4 A s2 s)))
                                  (SHIFT (B) s5))
                           (STATE s5 : (s : ((B s5 start s4 A s1 ())
                                             (B s5 start s4 A s2 s)))
                                  (REDUCE () r1))
                           (STATE s6 : (s : ((start s1 ())))
                                  (ACCEPT ($eos)))
                           (RULE r1 : (s : ((B s5 start s4 A s2 s)))
                                 start  (#f v2 #f) (+ 2 v2))
                           (RULE r2 : (s : ((B s2 A s2 s)))
                                 start  (#f #f) 2)))
              (make-pda '(A B $eos)
                        '$eos
                        's1
                        (list (make-state 's6 '(s : ((start s1 ())))
                                          (list)
                                          (list (make-accept '($eos)))
                                          (list))
                              (make-state 's5 '(s : ((B s5 start s4 A s1 ())
                                                     (B s5 start s4 A s2 s)))
                                          (list (make-reduce '() 'r1))
                                          (list)
                                          (list))
                              (make-state 's4 '(s : ((start s4 A s1 ())
                                                     (start s4 A s2 s)))
                                          (list (make-shift '(B) 's5))
                                          (list)
                                          (list))
                              (make-state 's3 '(s : ((B s2 A s1 ())
                                                     (B s2 A s2 s)))
                                          (list (make-reduce '() 'r2))
                                          (list)
                                          (list))
                              (make-state 's2 '(s : ((A s1 ())
                                                     (A s2 s)))
                                          (list (make-shift '(A) 's2)
                                                (make-shift '(B) 's3))
                                          (list)
                                          (list (make-goto 'start 's4)))
                              (make-state 's1 '(())
                                          (list (make-shift '(A) 's2))
                                          (list)
                                          (list (make-goto 'start 's6))))
                        (list (make-rule 'r2 '(s : ((B s2 A s2 s)))
                                         'start '(#f #f) '2)
                              (make-rule 'r1 '(s : ((B s5 start s4 A s2 s)))
                                         'start '(#f v2 #f) '(+ 2 v2)))))

(check-equal?
 (parse-pda
  '((TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
    (EOS *EOF*)
    (START s0)
    (ERROR *ERROR*)
    (NO-SHIFT *EOF*)
    (RULE r4 : (()) program (s-list #f) (cons (if #f #f) s-list))
    (RULE r5 : (()) s-list () '())
    (RULE r9 : (()) exp (NUM) NUM)
    (RULE r13 : (()) exp (expA #f exp) (quotient expA exp))
    (RULE r14 : (()) exp (#f exp #f) exp)
    (STATE
     s0 : (())
     (COMMENT s-list "=>" "." s-list statement)
     (COMMENT s-list "=>" ".")
     (COMMENT program "=>" "." s-list *ERROR*)
     (COMMENT program "=>" "." s-list exp)
     (COMMENT program "=>" "." s-list)
     (COMMENT *start "=>" "." program *EOF*)
     (REDUCE () r5)
     (GOTO program s1)
     (GOTO s-list s2))
    (STATE s1 : (()) (COMMENT *start "=>" program "." *EOF*) (ACCEPT (*EOF*)))
    (STATE
     s2 : (())
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
    (STATE s3  : (())
           (COMMENT s-list "=>" s-list statement ".") (REDUCE () r6))
    (STATE
     s4 : (())
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
 (make-pda '(NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
           '*EOF*
           's0
           (list (make-state
                  's4
                  '(())
                  (list
                   (make-shift '(SEMICOLON) 's19)
                   (make-shift '(TIMES) 's11)
                   (make-shift '(DIVIDE) 's12)
                   (make-shift '(PLUS) 's13)
                   (make-shift '(MINUS) 's14))
                  (list (make-reduce '(*EOF*) 'r3))
                  (list))
                 (make-state 's3 '(())
                             (list (make-reduce '() 'r6))
                             (list)
                             (list))
                 (make-state
                  's2
                  '(())
                  (list
                   (make-shift '(NUM) 's5)
                   (make-shift '(L-PAREN) 's6)
                   (make-shift '(*ERROR*) 's7))
                  (list (make-reduce '(*EOF*) 'r2))
                  (list (make-goto 'statement 's3) (make-goto 'exp 's4)))
                 (make-state 's1 '(())
                             (list)
                             (list (make-accept '(*EOF*)))
                             (list))
                 (make-state
                  's0
                  '(())
                  (list (make-reduce '() 'r5))
                  (list)
                  (list (make-goto 'program 's1) (make-goto 's-list 's2))))
           (list (make-rule 'r14 '(()) 'exp '(#f exp #f) 'exp)
                 (make-rule 'r13 '(()) 'exp '(expA #f exp) '(quotient expA exp))
                 (make-rule 'r9 '(()) 'exp '(NUM) 'NUM)
                 (make-rule 'r5 '(()) 's-list '() ''())
                 (make-rule 'r4 '(()) 'program '(s-list #f)
                            '(cons (if #f #f) s-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UnTyped PDAs

(check-equal? (parse-untyped-pda '((TOKENS A B $eos)
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
              (make-pda '(A B $eos)
                        '$eos
                        's1
                        (list (make-state 's6 #f
                                          (list)
                                          (list (make-accept '($eos)))
                                          (list))
                              (make-state 's5 #f
                                          (list (make-reduce '() 'r1))
                                          (list)
                                          (list))
                              (make-state 's4 #f
                                          (list (make-shift '(B) 's5))
                                          (list)
                                          (list))
                              (make-state 's3 #f
                                          (list (make-reduce '() 'r2))
                                          (list)
                                          (list))
                              (make-state 's2 #f
                                          (list (make-shift '(A) 's2)
                                                (make-shift '(B) 's3))
                                          (list)
                                          (list (make-goto 'start 's4)))
                              (make-state 's1 #f
                                          (list (make-shift '(A) 's2))
                                          (list)
                                          (list (make-goto 'start 's6))))
                        (list (make-rule 'r2 #f
                                         'start '(#f #f) '2)
                              (make-rule 'r1 #f
                                         'start '(#f v2 #f) '(+ 2 v2)))))

(check-equal?
 (parse-untyped-pda
  '((TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
    (EOS *EOF*)
    (START s0)
    (ERROR *ERROR*)
    (NO-SHIFT *EOF*)
    (RULE r4 program (s-list #f) (cons (if #f #f) s-list))
    (RULE r5 s-list () '())
    (RULE r9  exp (NUM) NUM)
    (RULE r13 exp (expA #f exp) (quotient expA exp))
    (RULE r14 exp (#f exp #f) exp)
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
    (STATE s1 (COMMENT *start "=>" program "." *EOF*) (ACCEPT (*EOF*)))
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
           (COMMENT s-list "=>" s-list statement ".") (REDUCE () r6))
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
     (REDUCE (*EOF*) r3))))
 (make-pda '(NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
           '*EOF*
           's0
           (list (make-state
                  's4
                  #f
                  (list
                   (make-shift '(SEMICOLON) 's19)
                   (make-shift '(TIMES) 's11)
                   (make-shift '(DIVIDE) 's12)
                   (make-shift '(PLUS) 's13)
                   (make-shift '(MINUS) 's14))
                  (list (make-reduce '(*EOF*) 'r3))
                  (list))
                 (make-state 's3 #f
                             (list (make-reduce '() 'r6))
                             (list)
                             (list))
                 (make-state
                  's2
                  #f
                  (list
                   (make-shift '(NUM) 's5)
                   (make-shift '(L-PAREN) 's6)
                   (make-shift '(*ERROR*) 's7))
                  (list (make-reduce '(*EOF*) 'r2))
                  (list (make-goto 'statement 's3) (make-goto 'exp 's4)))
                 (make-state 's1 #f
                             (list)
                             (list (make-accept '(*EOF*)))
                             (list))
                 (make-state
                  's0
                  #f
                  (list (make-reduce '() 'r5))
                  (list)
                  (list (make-goto 'program 's1) (make-goto 's-list 's2))))
           (list (make-rule 'r14 #f 'exp '(#f exp #f) 'exp)
                 (make-rule 'r13 #f 'exp '(expA #f exp) '(quotient expA exp))
                 (make-rule 'r9 #f 'exp '(NUM) 'NUM)
                 (make-rule 'r5 #f 's-list '() ''())
                 (make-rule 'r4 #f 'program '(s-list #f)
                            '(cons (if #f #f) s-list)))))