#lang racket
(require "../pda-data.rkt"
         (rename-in "../pdarisc-data.rkt"
                    [struct:state struct:risc-state]
                    [state? risc-state?]
                    [state risc-state]
                    [make-state make-risc-state]
                    [state-id risc-state-id]
                    [struct:accept struct:risc-accept]
                    [accept? risc-accept?]
                    [accept risc-accept]
                    [make-accept make-risc-accept]
                    [accept-vals risc-accept-vals])
         rackunit)
(require/expose "../compile-pda.rkt"
                (compile-pda gather-rto-table
                             add-reduce-to-pair
                             compile-state
                             compile-rule
                             compile-rule-args
                             compile-action
                             make-dict))

(check-equal?
 (compile-pda
  (make-pda '(A B plus $eos)
            '$eos
            's0
            (list (make-state 's0
                              '(())
                              (list (make-shift '(A) 's1)
                                    (make-reduce '(B) 'r1))
                              (list (make-goto 'nt 's1)))
                  (make-state 's1
                              '(())
                              (list (make-shift '(A) 's1)
                                    (make-reduce '() 'r1))
                              (list (make-goto 'nt 's0))))
            (list (make-rule 'r1 '(s : ((A s)))
                             'nt
                             '(v)
                             'v))))
 (make-pdarisc
  (list (make-label
         '(s0 s1 r1)
         '((()) (()) (s : ((A s)))) ; stacks
         '(#f #f #f) ; token-regs
         '(() () ()) ; arg-lists
         (list (list (make-block*
                      (list
                       (make-if-eos
                        (make-risc-accept '(reject))
                        (make-block*
                         (list
                          (make-get-token)
                          (make-token-case
                           '(A B)
                           (list
                            (make-block*
                             (list (make-push (make-risc-state 's0))
                                   (make-push (make-curr-token 0))
                                   (make-drop-token)
                                   (make-go 's1 '())))
                            (make-block*
                             (list (make-drop-token)
                                   (make-go 'r1'())))))))))))
               (list (make-block*
                      (list
                       (make-if-eos
                        (make-risc-accept '(reject))
                        (make-block*
                         (list
                          (make-get-token)
                          (make-token-case
                           '(A #t)
                           (list
                            (make-block*
                             (list (make-push (make-risc-state 's1))
                                   (make-push (make-curr-token 0))
                                   (make-drop-token)
                                   (make-go 's1 '())))
                            (make-block*
                             (list (make-drop-token)
                                   (make-go 'r1 '())))))))))))
               (list (make-block*
                      (list
                       (make-assign 'v (make-pop))
                       (make-assign 'target (make-pop)) ; state
                       (make-sem-act '(v) '(ret-val) 'v)
                       (make-push (make-var-ref 'target))
                       (make-push (make-var-ref 'ret-val))
                       (make-state-case (make-var-ref 'target)
                                        '(s0 s1)
                                        (list (list (make-go 's1 '()))
                                              (list (make-go 's0 '()))))))))
         (list (make-go 's0 '()))))))



(check-equal? (gather-rto-table
               (list (make-state 's0
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list (make-goto 'nt 's1)))
                     (make-state 's1
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list (make-goto 'nt 's0)))))
              (make-dict 'nt (make-dict 's0 's1 's1 's0)))

(check-equal? (gather-rto-table
               (list (make-state 's0
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list (make-goto 'nt 's1)))
                     (make-state 's1
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list (make-goto 'nt2 's0)))))
              (make-dict 'nt (make-dict 's0 's1)
                         'nt2 (make-dict 's1 's0)))


(check-equal? (add-reduce-to-pair (make-goto 'nt 's1)
                                  's0
                                  (make-dict 'nt (make-dict)))
              (make-dict 'nt (make-dict 's0 's1)))

(check-equal? (add-reduce-to-pair (make-goto 'nt 's1)
                                  's0
                                  (make-dict))
              (make-dict 'nt (make-dict 's0 's1)))

(check-equal? (add-reduce-to-pair (make-goto 'nt 's0)
                                  's0
                                  (make-dict 'nt (make-dict)
                                             'nt2 (make-dict)))
              (make-dict 'nt (make-dict 's0 's0)
                         'nt2 (make-dict)))

(check-equal? (add-reduce-to-pair (make-goto 'nt2 's3)
                                  's2
                                  (make-dict 'nt1 (make-dict 's0 's0)
                                             'nt2 (make-dict)))
              (make-dict 'nt1 (make-dict 's0 's0)
                         'nt2 (make-dict 's2 's3)))

(check-equal? (compile-state (make-state 's0
                                         '(())
                                         (list (make-shift '(A) 's1)
                                               (make-reduce '(B) 'r1))
                                         (list (make-goto 'nt 's1))))
              (make-block*
               (list
                (make-if-eos
                 (make-risc-accept '(reject))
                 (make-block*
                  (list
                   (make-get-token)
                   (make-token-case
                    '(A B)
                    (list
                     (make-block* (list (make-push (make-risc-state 's0))
                                        (make-push (make-curr-token 0))
                                        (make-drop-token)
                                        (make-go 's1 '())))
                     (make-block* (list (make-drop-token)
                                        (make-go 'r1 '())))))))))))

(check-equal? (compile-rule (make-rule 'r1
                                       '((int plus int))
                                       'nt
                                       '(v1 #f v2)
                                       '(+ v1 v2))
                            (make-dict 'nt (make-dict 's1 's2
                                                      's2 's0)))
              (make-block*
               (list (make-assign 'v2 (make-pop))
                     (make-assign '_ (make-pop)) ; state
                     (make-assign '_ (make-pop))
                     (make-assign '_ (make-pop)) ; state
                     (make-assign 'v1 (make-pop))
                     (make-assign 'target (make-pop)) ; state
                     (make-sem-act '(v1 v2) '(ret-val) '(+ v1 v2))
                     (make-push (make-var-ref 'target))
                     (make-push (make-var-ref 'ret-val))
                     (make-state-case (make-var-ref 'target)
                                      '(s1 s2)
                                      (list (list (make-go 's2 '()))
                                            (list (make-go 's0 '())))))))

(check-equal? (compile-rule (make-rule 'r1 '(s : ((A s))) 'nt '(v) 'v)
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign 'v (make-pop))
                     (make-assign 'target (make-pop)) ; state
                     (make-sem-act '(v) '(ret-val) 'v)
                     (make-push (make-var-ref 'target))
                     (make-push (make-var-ref 'ret-val))
                     (make-state-case (make-var-ref 'target)
                                      '(s0 s1)
                                      (list (list (make-go 's1 '()))
                                            (list (make-go 's0 '())))))))

(check-equal? (compile-rule-args '(v1 #f v2))
              (list (make-assign 'v2 (make-pop))
                    (make-assign '_ (make-pop)) ; state
                    (make-assign '_ (make-pop))
                    (make-assign '_ (make-pop)) ; state
                    (make-assign 'v1 (make-pop))
                    (make-assign 'target (make-pop))))

(check-equal? (compile-rule-args '(v))
              (list (make-assign 'v (make-pop))
                    (make-assign 'target (make-pop))))

(check-equal? (compile-rule-args '())
              (list (make-assign 'v (make-pop))
                    (make-assign 'target (make-pop))
                    (make-push (make-var-ref 'v))
                    (make-push (make-var-ref 'target))))
(check-equal? (compile-action (make-shift '(A) 's1) 's0)
              (make-block* (list (make-push (make-risc-state 's0))
                                 (make-push (make-curr-token 0))
                                 (make-drop-token)
                                 (make-go 's1 '()))))
(check-equal? (compile-action (make-reduce '(B) 'r1) 's0)
              (make-block* (list (make-drop-token)
                                 (make-go 'r1 '()))))
(check-equal? (compile-action (make-reduce '() 'r1) 's0)
              (make-block* (list (make-drop-token)
                                 (make-go 'r1 '()))))
(check-equal? (compile-action (make-goto 'nt 's1) 's0)
              (make-block* (list (make-push (make-risc-state 's0))
                                 (make-push (make-nterm 'nt))
                                 (make-go 's1 '()))))
(check-equal? (compile-action (make-accept '(A)) 's0)
              (make-block* (list (make-risc-accept '()))))