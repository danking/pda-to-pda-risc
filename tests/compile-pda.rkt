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
                             compile-eos-state
                             compile-rule
                             compile-rule-args
                             compile-action
                             make-dict
                             action-has-no-lookahead?))

(check-equal?
 (compile-pda
  (make-pda '(A B plus $eos)
            '$eos
            's0
            (list (make-state 's0
                              '(())
                              (list (make-shift '(A) 's1)
                                    (make-reduce '(B) 'r1))
                              (list)
                              (list (make-goto 'nt 's1)))
                  (make-state 's1
                              '(())
                              (list (make-shift '(A) 's1)
                                    (make-reduce '() 'r1))
                              (list)
                              (list (make-goto 'nt 's0))))
            (list (make-rule 'r1 '(s : ((A s)))
                             'nt
                             '(v)
                             'v))))
 (make-pdarisc
  (list (make-label
         (list (make-label-polynym 's0 'unknown)
               (make-label-polynym 's1 'unknown)
               (make-label-polynym 's0 'have-token)
               (make-label-polynym 's1 'have-token)
               (make-label-polynym 's0 'eos)
               (make-label-polynym 's1 'eos)
               (make-label-polynym 'r1 'have-token)
               (make-label-polynym 'r1 'eos))
         '((()) (()) (()) (()) (()) (()) (s : ((A s))) (s : ((A s)))) ; stacks
         '(#f #f #f #f #f #f #f #f) ; token-regs
         '(() () () () () () () ()) ; arg-lists
         (list (list (make-block*
                      (list
                       (make-if-eos
                        (make-go (make-label-polynym 's0 'eos) '())
                        (make-block*
                         (list
                          (make-get-token)
                          (make-go (make-label-polynym 's0 'have-token)
                                   '())))))))
               (list (make-block*
                      (list
                       (make-if-eos
                        (make-go (make-label-polynym 's1 'eos) '())
                        (make-block*
                         (list
                          (make-get-token)
                          (make-go (make-label-polynym 's1 'have-token)
                                   '())))))))
               (list (make-block*
                      (list (make-push (make-risc-state 's0))
                            (make-token-case
                             '(A B #f)
                             (list
                              (list (make-push (make-curr-token #f))
                                    (make-drop-token)
                                    (make-go (make-label-polynym 's1 'unknown)
                                             '()))
                              (list (make-go (make-label-polynym 'r1
                                                                 'have-token)
                                             '()))
                              (list (make-reject)))))))
               (list (make-block*
                      (list (make-push (make-risc-state 's1))
                            (make-token-case
                             '(A #f)
                             (list
                              (list (make-push (make-curr-token #f))
                                    (make-drop-token)
                                    (make-go (make-label-polynym 's1 'unknown)
                                             '()))
                              (list (make-go (make-label-polynym 'r1
                                                                 'have-token)
                                             '())))))))
               (list (make-block*
                      (list (make-push (make-risc-state 's0))
                            (make-reject))))
               (list (make-block*
                      (list (make-push (make-risc-state 's1))
                            (make-go (make-label-polynym 'r1
                                                         'eos)
                                     '()))))
               (list (make-block*
                      (list
                       (make-assign (make-nameless-reg) (make-pop))
                       (make-assign (make-named-reg 'v) (make-pop))
                       (make-assign (make-named-reg 'target) (make-pop)) ; state
                       (make-sem-act 'r1
                                     (list (make-named-reg 'v))
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
               (list (make-block*
                      (list
                       (make-assign (make-nameless-reg) (make-pop))
                       (make-assign (make-named-reg 'v) (make-pop))
                       (make-assign (make-named-reg 'target) (make-pop)) ; state
                       (make-sem-act 'r1
                                     (list (make-named-reg 'v))
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
                                                               'eos)
                                           '()))
                                         (list
                                          (make-go
                                           (make-label-polynym 's0
                                                               'eos)
                                           '()))))))))
         (list (make-go (make-label-polynym 's0 'unknown) '()))))))



(check-equal? (gather-rto-table
               (list (make-state 's0
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list)
                                 (list (make-goto 'nt 's1)))
                     (make-state 's1
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list)
                                 (list (make-goto 'nt 's0)))))
              (make-dict 'nt (make-dict 's0 's1 's1 's0)))

(check-equal? (gather-rto-table
               (list (make-state 's0
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list)
                                 (list (make-goto 'nt 's1)))
                     (make-state 's1
                                 '(())
                                 (list (make-shift '(A) 's1)
                                       (make-reduce '(B) 'r1))
                                 (list)
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
                                         (list)
                                         (list (make-goto 'nt 's1))))
              (make-block*
               (list
                (make-if-eos
                 (make-go (make-label-polynym 's0 'eos) '())
                 (make-block*
                  (list
                   (make-get-token)
                   (make-go (make-label-polynym 's0 'have-token) '())))))))

(check-equal? (compile-have-token-state
               (make-state 's0
                           '(())
                           (list (make-shift '(A) 's1)
                                 (make-reduce '(B) 'r1))
                           (list)
                           (list (make-goto 'nt 's1))))
              (make-block*
               (list
                (make-push (make-risc-state 's0))
                (make-token-case
                 '(A B #f)
                 (list
                  (list (make-push (make-curr-token #f))
                        (make-drop-token)
                        (make-go (make-label-polynym 's1 'unknown)
                                 '()))
                  (list (make-go (make-label-polynym 'r1
                                                     'have-token)
                                 '()))
                  (list (make-reject)))))))

(check-equal? (compile-have-token-state
               (make-state 's1
                           '(())
                           (list (make-shift '(A) 's1)
                                 (make-reduce '() 'r1))
                           (list)
                           (list (make-goto 'nt 's0))))
              (make-block*
               (list
                (make-push (make-risc-state 's1))
                (make-token-case
                 '(A #f)
                 (list
                  (list (make-push (make-curr-token #f))
                        (make-drop-token)
                        (make-go (make-label-polynym 's1 'unknown)
                                 '()))
                  (list (make-go (make-label-polynym 'r1
                                                     'have-token)
                                 '())))))))

(check-equal? (compile-eos-state (make-state 's0
                                             '(())
                                             (list (make-shift '(A) 's1)
                                                   (make-reduce '(B) 'r1))
                                             (list)
                                             (list (make-goto 'nt 's1))))
              (make-block*
               (list
                (make-push (make-risc-state 's0))
                (make-reject))))

(check-equal? (compile-eos-state (make-state 's1
                                             '(())
                                             (list (make-shift '(A) 's1)
                                                   (make-reduce '() 'r1))
                                             (list)
                                             (list (make-goto 'nt 's0))))
              (make-block*
               (list (make-push (make-risc-state 's1))
                     (make-go (make-label-polynym 'r1 'eos) '()))))

(check-equal? (compile-eos-state (make-state 's1
                                             '(())
                                             (list (make-shift '(A) 's1)
                                                   (make-reduce '() 'r1)
                                                   (make-reduce '() 'r3))
                                             (list)
                                             (list (make-goto 'nt 's0))))
              (make-block*
               (list (make-push (make-risc-state 's1))
                     (make-go (make-label-polynym 'r1 'eos) '()))))

(check-equal? (compile-rule (make-rule 'r1
                                       '((int plus int))
                                       'nt
                                       '(v1 #f v2)
                                       '(+ v1 v2))
                            'have-token
                            (make-dict 'nt (make-dict 's1 's2
                                                      's2 's0)))
              (make-block*
               (list (make-assign (make-nameless-reg) (make-pop))
                     (make-assign (make-named-reg 'v2)  (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-nameless-reg)   (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-named-reg 'v1)  (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list (make-named-reg 'v1)
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

(check-equal? (compile-rule (make-rule 'r1
                                       '((int plus int))
                                       'nt
                                       '(v1 #f v2)
                                       '(+ v1 v2))
                            'eos
                            (make-dict 'nt (make-dict 's1 's2
                                                      's2 's0)))
              (make-block*
               (list (make-assign (make-nameless-reg) (make-pop))
                     (make-assign (make-named-reg 'v2)  (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-nameless-reg)   (make-pop))
                     (make-assign (make-nameless-reg)   (make-pop)) ; state
                     (make-assign (make-named-reg 'v1)  (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list (make-named-reg 'v1)
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
                                                             'eos)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'eos)
                                         '())))))))

(check-equal? (compile-rule (make-rule 'r1 '(s : ((A s))) 'nt '(v) 'v)
                            'have-token
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign (make-nameless-reg) (make-pop))
                     (make-assign (make-named-reg 'v) (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list (make-named-reg 'v))
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

(check-equal? (compile-rule (make-rule 'r1 '(s : ((A s))) 'nt '(v) 'v)
                            'eos
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign (make-nameless-reg) (make-pop))
                     (make-assign (make-named-reg 'v) (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list (make-named-reg 'v))
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
                                                             'eos)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'eos)
                                         '())))))))

(check-equal? (compile-rule (make-rule 'r1 '(s : ((A s))) 'nt '(v) 'v)
                            'eos
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign (make-nameless-reg) (make-pop))
                     (make-assign (make-named-reg 'v) (make-pop))
                     (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list (make-named-reg 'v))
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
                                                             'eos)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'eos)
                                         '())))))))

(check-equal? (compile-rule (make-rule 'r1 '((s1) (s0)) 'nt '() ''mt)
                            'eos
                            (make-dict 'nt (make-dict 's0 's1
                                                      's1 's0)))
              (make-block*
               (list (make-assign (make-named-reg 'target) (make-pop)) ; state
                     (make-sem-act 'r1
                                   (list)
                                   (list (make-named-reg 'ret-val))
                                   ''mt)
                     (make-push (make-named-reg 'target))
                     (make-push (make-named-reg 'ret-val))
                     (make-state-case (make-named-reg 'target)
                                      (list (make-risc-state 's0)
                                            (make-risc-state 's1))
                                      (list
                                       (list
                                        (make-go
                                         (make-label-polynym 's1
                                                             'eos)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-polynym 's0
                                                             'eos)
                                         '())))))))

(check-equal? (compile-rule-args '(v1 #f v2))
              (list (make-assign (make-nameless-reg) (make-pop))
                    (make-assign (make-named-reg 'v2) (make-pop))
                    (make-assign (make-nameless-reg)  (make-pop)) ; state
                    (make-assign (make-nameless-reg)  (make-pop))
                    (make-assign (make-nameless-reg)  (make-pop)) ; state
                    (make-assign (make-named-reg 'v1) (make-pop))
                    (make-assign (make-named-reg 'target) (make-pop))))

(check-equal? (compile-rule-args '(v))
              (list (make-assign (make-nameless-reg) (make-pop))
                    (make-assign (make-named-reg 'v) (make-pop))
                    (make-assign (make-named-reg 'target) (make-pop))))

(check-equal? (compile-rule-args '())
              (list (make-assign (make-named-reg 'target) (make-pop))))

(check-equal? (compile-action (make-shift '(A) 's1) 's0 #f)
              (list (make-push (make-curr-token #f))
                    (make-drop-token)
                    (make-go (make-label-polynym 's1 'unknown) '())))
(check-equal? (compile-action (make-reduce '(B) 'r1) 's0 #f)
              (list (make-go (make-label-polynym 'r1 'have-token) '())))
(check-equal? (compile-action (make-reduce '() 'r1) 's0 #f)
              (list (make-go (make-label-polynym 'r1 'have-token) '())))
(check-equal? (compile-action (make-accept '(A)) 's0 #f)
              (list (make-assign (make-nameless-reg) (make-pop))
                    (make-assign (make-named-reg 'result) (make-pop))
                    (make-risc-accept (list (make-named-reg 'result)))))

(check-exn exn:fail?
           (lambda ()
             (compile-action (make-shift '(A) 's1) 's0 #t)))
(check-equal? (compile-action (make-reduce '(B) 'r1) 's0 #t)
              (list (make-go (make-label-polynym 'r1 'eos) '())))
(check-equal? (compile-action (make-reduce '() 'r1) 's0 #t)
              (list (make-go (make-label-polynym 'r1 'eos) '())))
(check-equal? (compile-action (make-accept '(A)) 's0 #t)
              (list (make-assign (make-nameless-reg) (make-pop))
                    (make-assign (make-named-reg 'result) (make-pop))
                    (make-risc-accept (list (make-named-reg 'result)))))

(check-equal? (action-has-no-lookahead? (make-accept '(A))) #f)
(check-equal? (action-has-no-lookahead? (make-accept '())) #t)
(check-equal? (action-has-no-lookahead? (make-goto '(A) 's0)) #f)
(check-equal? (action-has-no-lookahead? (make-goto '() 's0)) #t)
(check-equal? (action-has-no-lookahead? (make-reduce '(A) 'r1)) #f)
(check-equal? (action-has-no-lookahead? (make-reduce '() 'r1)) #t)
(check-equal? (action-has-no-lookahead? (make-shift '(A) 's0)) #f)
(check-equal? (action-has-no-lookahead? (make-shift '() 's0)) #t)
