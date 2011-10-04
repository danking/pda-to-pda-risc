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
                             compile-have-token-state
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
         (list (make-label-name 's0)
               (make-label-name 's1)
               (make-label-polynym 's0 'have-token)
               (make-label-polynym 's1 'have-token)
               (make-label-name 'r1))
         '((()) (()) (()) (()) (s : ((A s)))) ; stacks
         '(#f #f #f #f #f) ; token-regs
         '(() () () () ()) ; arg-lists
         (list (list (make-block*
                      (list
                       (make-if-eos
                        (make-risc-accept (list (make-named-reg 'reject)))
                        (make-block*
                         (list
                          (make-get-token)
                          (make-go (make-label-polynym 's0 'have-token)
                                   '())))))))
               (list (make-block*
                      (list
                       (make-if-eos
                        (make-risc-accept (list (make-named-reg 'reject)))
                        (make-block*
                         (list
                          (make-get-token)
                          (make-go (make-label-polynym 's1 'have-token)
                                   '())))))))
               (list (make-block*
                      (list (make-token-case
                             '(A B)
                             (list
                              (list (make-push (make-risc-state 's0))
                                    (make-push (make-curr-token #f))
                                    (make-drop-token)
                                    (make-go (make-label-polynym 's1
                                                                 'unknown)
                                             '()))
                              (list (make-go (make-label-polynym 'r1
                                                                 'have-token)
                                             '())))))))
               (list (make-block*
                      (list (make-token-case
                           '(A #t)
                           (list
                            (list (make-push (make-risc-state 's1))
                                  (make-push (make-curr-token #f))
                                  (make-drop-token)
                                  (make-go (make-label-polynym 's1
                                                               'unknown)
                                           '()))
                            (list (make-go (make-label-polynym 'r1
                                                               'have-token)
                                           '())))))))
               (list (make-block*
                      (list
                       (make-assign (make-named-reg 'v) (make-pop))
                       (make-assign (make-named-reg 'target) (make-pop)) ; state
                       (make-sem-act (list (make-named-reg 'v))
                                     (list (make-named-reg 'ret-val))
                                     'v)
                       (make-push (make-named-reg 'target))
                       (make-push (make-named-reg 'ret-val))
                       (make-state-case (make-named-reg 'target)
                                        (list (make-risc-state 's0)
                                              (make-risc-state 's1))
                                        (list
                                         (list
                                          (make-go
                                           (make-label-polynym 's1
                                                               'have-token)
                                           '()))
                                         (list
                                          (make-go
                                           (make-label-polynym 's0
                                                               'have-token)
                                           '()))))))))
         (list (make-go (make-label-name 's0) '()))))))



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
                 (make-risc-accept (list (make-named-reg 'reject)))
                 (make-block*
                  (list
                   (make-get-token)
                   (make-go (make-label-polynym 's0 'have-token) '())))))))

(check-equal? (compile-have-token-state
               (make-state 's0
                           '(())
                           (list (make-shift '(A) 's1)
                                 (make-reduce '(B) 'r1))
                           (list (make-goto 'nt 's1))))
              (make-block*
               (list
                (make-token-case
                 '(A B)
                 (list
                  (list (make-push (make-risc-state 's0))
                        (make-push (make-curr-token #f))
                        (make-drop-token)
                        (make-go (make-label-polynym 's1
                                                     'unknown)
                                 '()))
                  (list (make-go (make-label-polynym 'r1
                                                     'have-token)
                                 '())))))))

(check-equal? (compile-rule (make-rule 'r1
                                       '((int plus int))
                                       'nt
                                       '(v1 #f v2)
                                       '(+ v1 v2))
                            (make-dict 'nt (make-dict 's1 's2
                                                      's2 's0)))
              (make-block*
               (list (make-assign (make-named-reg 'v2)  (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-nameless-reg)   (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-named-reg 'v1)  (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act (list (make-named-reg 'v1)
                                         (make-named-reg 'v2))
                                   (list (make-named-reg 'ret-val))
                                   '(+ v1 v2))
                     (make-push (make-named-reg 'target))
                     (make-push (make-named-reg 'ret-val))
                     (make-state-case (make-named-reg 'target)
                                      (list (make-risc-state 's1)
                                            (make-risc-state 's2))
                                      (list
                                       (list
                                        (make-go
                                         (make-label-polynym 's2
                                                             'have-token)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'have-token)
                                         '())))))))

(check-equal? (compile-rule (make-rule 'r1 '(s : ((A s))) 'nt '(v) 'v)
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign (make-named-reg 'v) (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act (list (make-named-reg 'v))
                                   (list (make-named-reg 'ret-val))
                                   'v)
                     (make-push (make-named-reg 'target))
                     (make-push (make-named-reg 'ret-val))
                     (make-state-case (make-named-reg 'target)
                                      (list (make-risc-state 's0)
                                            (make-risc-state 's1))
                                      (list
                                       (list
                                        (make-go
                                         (make-label-polynym 's1
                                                             'have-token)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'have-token)
                                         '())))))))

(check-equal? (compile-rule-args '(v1 #f v2))
              (list (make-assign (make-named-reg 'v2) (make-pop))
                    (make-assign (make-nameless-reg)  (make-pop)) ; state
                    (make-assign (make-nameless-reg)  (make-pop))
                    (make-assign (make-nameless-reg)  (make-pop)) ; state
                    (make-assign (make-named-reg 'v1) (make-pop))
                    (make-assign (make-named-reg 'target) (make-pop))))

(check-equal? (compile-rule-args '(v))
              (list (make-assign (make-named-reg 'v) (make-pop))
                    (make-assign (make-named-reg 'target) (make-pop))))

(check-equal? (compile-rule-args '())
              (list (make-assign (make-named-reg 'v) (make-pop))
                    (make-assign (make-named-reg 'target) (make-pop))
                    (make-push (make-named-reg 'v))
                    (make-push (make-named-reg 'target))))
(check-equal? (compile-action (make-shift '(A) 's1) 's0)
              (list (make-push (make-risc-state 's0))
                    (make-push (make-curr-token #f))
                    (make-drop-token)
                    (make-go (make-label-polynym 's1 'unknown) '())))
(check-equal? (compile-action (make-reduce '(B) 'r1) 's0)
              (list (make-go (make-label-polynym 'r1 'have-token) '())))
(check-equal? (compile-action (make-reduce '() 'r1) 's0)
              (list (make-go (make-label-polynym 'r1 'have-token) '())))
(check-equal? (compile-action (make-goto 'nt 's1) 's0)
              (list (make-push (make-risc-state 's0))
                    (make-push (make-nterm 'nt))
                    (make-go (make-label-polynym 's1 'have-token) '())))
(check-equal? (compile-action (make-accept '(A)) 's0)
              (list (make-risc-accept '())))
