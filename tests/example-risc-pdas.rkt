#lang racket
(require "../pda-data.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAMPLE DATA USED IN TESTS
(define pda1
  '((TOKENS A B $eos)
    (EOS $eos)
    (START s1)
    (STATE s1 (SHIFT (A) s2) (GOTO start s6))
    (STATE s2 (SHIFT (A) s2) (SHIFT (B) s3) (GOTO start s4))
    (STATE s3 (REDUCE () r2))
    (STATE s4 (SHIFT (B) s5))
    (STATE s5 (REDUCE () r1))
    (STATE s6 (ACCEPT ($eos)))
    (RULE r1 start  (#f v2 #f) (+ 2 v2))
    (RULE r2 start  (#f #f) 2)
    (RULE r3 accept (v1 #f) v1)))



(define pda1-risc-struct
  (make-pda '(A B $eos)
            '$eos
            's1
            (list (make-state 's6 (list (make-accept '$eos)) '())
                  (make-state 's5 (list (make-reduce #t 'r1)) '())
                  (make-state 's4 (list (make-shift 'B 's5)) '())
                  (make-state 's3 (list (make-reduce #t 'r2)) '())
                  (make-state 's2
                              (list (make-shift 'A 's2) (make-shift 'B 's3))
                              (list (make-goto 'start 's4)))
                  (make-state 's1
                              (list (make-shift 'A 's2))
                              (list (make-goto 'start 's6))))
            (list (make-rule 'r3 'accept 2 'v1)
                  (make-rule 'r2 'start 2 '2)
                  (make-rule 'r1 'start 3 '(+ 2 v2)))))

(define pda1-risc
  '(label ((s1 ()
               (if-eos (go s1-eos)
                       (block get-token
                              (go s1-have-token))))
           (s1-eos ())
           (s1-have-token ()
                          (token-case
                           (A (block (push (state s1))
                                     (push (current-token))
                                     drop-token
                                     (go s2)))))
           (s2 ()
               (if-eos (go s2-eos)
                       (block get-token
                              (go s2-have-token))))
           (s2-eos ())
           (s2-have-token ()
                          (token-case
                           (A (block (push (state s2))
                                     (push (current-token))
                                     drop-token
                                     (go s2)))
                           (B (block (push (state s2))
                                     (push (current-token))
                                     drop-token
                                     (go s3)))))

           (s3 ()
               (if-eos (go s3-eos)
                       (block get-token
                              (go s3-have-token))))
           (s3-eos ()
                   (go r2-eos))
           (s3-have-token ()
                          (token-case
                           (#t (go r2))))

           (s4 ()
               (if-eos (go s4-eos)
                       (block get-token
                              (go s4-have-token))))
           (s4-eos ())
           (s4-have-token ()
                          (token-case
                           (B (block (push (state s4))
                                     (push (current-token))
                                     drop-token
                                     (go s5)))))

           (s5 ()
               (if-eos (go s5-eos)
                       (block get-token
                              (go s5-have-token))))
           (s5-eos () (go r1-eos))
           (s5-have-token ()
                          (token-case
                           (#t (go r1))))

           (s6 ()
               (if-eos (go s6-eos)
                       (block get-token
                              (go s6-have-token))))
           (s6-eos ()
                   (block (:= final-semantic-value (pop))
                          (accept final-semantic-value)))
           (s6-have-token () (token-case))

           (r1 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (:= v3 (pop))
               (:= st3 (pop))
               (semantic-action (v1 v2 v3) (result) (+ 2 v2))
               (push st3)
               (push result)
               (state-case st3
                           (s1 (go s6-have-token))
                           (s2 (go s4-have-token))))

           (r1-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (:= v3 (pop))
                   (:= st3 (pop))
                   (semantic-action (v1 v2 v3) (result) (+ 2 v2))
                   (push st3)
                   (push result)
                   (state-case st3
                               (s1 (go s6-eos))
                               (s2 (go s4-eos))))

           (r2 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (semantic-action (v1 v2) (result) 2)
               (push st2)
               (push result)
               (state-case st2
                           (s1 (go s6-have-token))
                           (s2 (go s4-have-token))))

           (r2-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (semantic-action (v1 v2) (result) 2)
                   (push st2)
                   (push result)
                   (state-case st2
                               (s1 (go s6-eos))
                               (s2 (go s4-eos))))
           (r3 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (semantic-action (v1 v2) (result) v1)
               (push st2)
               (push result)
               (state-case st2))

           (r3-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (semantic-action (v1 v2) (result) v1)
                   (push st2)
                   (push result)
                   (state-case st2)))
       (go s1)))

(define calculator-pda
  '((TOKENS NUM L-PAREN R-PAREN SEMICOLON TIMES DIVIDE PLUS MINUS *EOF*)
    (ERROR *ERROR*)
    (EOS *EOF*)
    (NO-SHIFT *EOF*)
    (START s2)
    (RULE r1 *start 2 #f)
    (RULE r2 program 1 v1)
    (RULE r3 program 2 (begin (display v2) (newline) (cons v2 v1)))
    (RULE r4 program 2 (cons (if #f #f) v1))
    (RULE r5 s-list 0 '())
    (RULE r6 s-list 2 (cons v2 v1))
    (RULE r7 statement 2 (begin (display v1) (newline) v1))
    (RULE r8 statement 2 (if #f #f))
    (RULE r9 exp 1 v1)
    (RULE r10 exp 3 (+ v1 v3))
    (RULE r11 exp 3 (- v1 v3))
    (RULE r12 exp 3 (* v1 v3))
    (RULE r13 exp 3 (quotient v1 v3))
    (RULE r14 exp 3 v2)
    (STATE s0 (REDUCE () r5) (GOTO program s1) (GOTO s-list s2))
    (STATE s1 (ACCEPT (*EOF*)))
    (STATE
     s2
     (SHIFT (NUM) s5)
     (SHIFT (L-PAREN) s6)
     (SHIFT (*ERROR*) s7)
     (REDUCE (*EOF*) r2)
     (GOTO statement s3)
     (GOTO exp s4))
    (STATE s3 (REDUCE () r6))
    (STATE
     s4
     (SHIFT (SEMICOLON) s19)
     (SHIFT (TIMES) s11)
     (SHIFT (DIVIDE) s12)
     (SHIFT (PLUS) s13)
     (SHIFT (MINUS) s14)
     (REDUCE (*EOF*) r3))
    (STATE s5 (REDUCE () r9))
    (STATE s6 (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s9))
    (STATE s7 (SHIFT (SEMICOLON) s8) (REDUCE (*EOF*) r4))
    (STATE s8 (REDUCE () r8))
    (STATE
     s9
     (SHIFT (R-PAREN) s10)
     (SHIFT (TIMES) s11)
     (SHIFT (DIVIDE) s12)
     (SHIFT (PLUS) s13)
     (SHIFT (MINUS) s14))
    (STATE s10 (REDUCE () r14))
    (STATE s11 (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s18))
    (STATE s12 (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s17))
    (STATE s13 (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s16))
    (STATE s14 (SHIFT (NUM) s5) (SHIFT (L-PAREN) s6) (GOTO exp s15))
    (STATE
     s15
     (REDUCE (R-PAREN) r11)
     (REDUCE (SEMICOLON) r11)
     (SHIFT (TIMES) s11)
     (SHIFT (DIVIDE) s12)
     (REDUCE (PLUS) r11)
     (REDUCE (MINUS) r11)
     (REDUCE (*EOF*) r11))
    (STATE
     s16
     (REDUCE (R-PAREN) r10)
     (REDUCE (SEMICOLON) r10)
     (SHIFT (TIMES) s11)
     (SHIFT (DIVIDE) s12)
     (REDUCE (PLUS) r10)
     (REDUCE (MINUS) r10)
     (REDUCE (*EOF*) r10))
    (STATE
     s17
     (REDUCE (R-PAREN) r13)
     (REDUCE (SEMICOLON) r13)
     (REDUCE (TIMES) r13)
     (REDUCE (DIVIDE) r13)
     (REDUCE (PLUS) r13)
     (REDUCE (MINUS) r13)
     (REDUCE (*EOF*) r13))
    (STATE
     s18
     (REDUCE (R-PAREN) r12)
     (REDUCE (SEMICOLON) r12)
     (REDUCE (TIMES) r12)
     (REDUCE (DIVIDE) r12)
     (REDUCE (PLUS) r12)
     (REDUCE (MINUS) r12)
     (REDUCE (*EOF*) r12))
    (STATE s19 (REDUCE () r7))
    (STATE s20 (REDUCE () r1))))
