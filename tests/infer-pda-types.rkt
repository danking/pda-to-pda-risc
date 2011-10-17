#lang racket
(require rackunit
         "../pda-data.rkt"
         "../parse-pda.rkt")
(require/expose "../infer-pda-types.rkt"
                (infer-pda-types get-edges))

(check-equal? (infer-pda-types (parse-untyped-pda '((TOKENS A B $eos)
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
                                                          start
                                                          (#f v2 #f) (+ 2 v2))
                                                    (RULE r2
                                                          start
                                                          (#f #f) 2))))
              (pda
               '(A B $eos)
               '$eos
               's1
               (list
                (state 's6 '((start s1)) '() (list (accept '($eos))) '())
                (state
                 's5
                 '((B s4 start s2 A s2) (B s4 start s2 A s1))
                 (list (reduce '() 'r1))
                 '()
                 '())
                (state
                 's4
                 '((start s2 A s2) (start s2 A s1))
                 (list (shift '(B) 's5))
                 '()
                 '())
                (state 's3 '((B s2 A s2) (B s2 A s1))
                       (list (reduce '() 'r2)) '() '())
                (state
                 's2
                 '((A s2) (A s1))
                 (list (shift '(A) 's2) (shift '(B) 's3))
                 '()
                 (list (goto 'start 's4)))
                (state 's1 '(())
                       (list (shift '(A) 's2)) '() (list (goto 'start 's6))))
               (list
                (rule 'r2 '((B s2 A s2) (B s2 A s1)) 'start '(#f #f) 2)
                (rule
                 'r1
                 '((B s4 start s2 A s2) (B s4 start s2 A s1))
                 'start
                 '(#f v2 #f)
                 '(+ 2 v2)))))

(check-equal? (get-edges (make-state 's2 '(s : ((A s1 ())
                                                (A s2 s)))
                                     (list (make-shift '(A) 's2)
                                           (make-shift '(B) 's3))
                                     (list)
                                     (list (make-goto 'start 's4))))
              '((s4 start) (s3 B) (s2 A)))

(check-equal? (map get-edges (list
                              (make-state 's6 '(s : ((start s1 ())))
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
                                          (list (make-goto 'start 's6)))))
              '(()
                ()
                ((s5 B))
                ()
                ((s4 start) (s3 B) (s2 A))
                ((s6 start) (s2 A))))

