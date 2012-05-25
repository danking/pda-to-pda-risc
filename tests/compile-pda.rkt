#lang racket
(require (only-in "../pda-data.rkt"
                  pda-states
                  make-shift
                  make-reduce
                  make-goto
                  [make-accept make-pda-accept])
         "../parse-pda.rkt"
         "../pdarisc-data.rkt"
         "../unparse-pdarisc.rkt"
         "test-util.rkt"
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


(define-test-suite
  compile-pda-tests
  (test-case
   "simple-pda"
   (define simple-pda
     (parse-pda
      #'(parse-pda (TOKENS A B plus $eos)
                   (EOS $eos)
                   (START s0)
                   (STATE s0 (())
                          (SHIFT (A) s1)
                          (REDUCE (B) r1)
                          (GOTO nt s1))
                   (STATE s1 (())
                          (SHIFT (A) s1)
                          (REDUCE () r1)
                          (GOTO nt2 s0))
                   (RULE r1 ((A s)) nt (v) v)
                   (RULE r2 (()) nt2 (v) v))))
   (check-syntax-equal?
    (unparse-pdarisc (compile-pda simple-pda))
    '((label ((s0-unknown (()) #f
                          ()
                          (block (if-eos (go s0-eos)
                                         (block get-token
                                                (go s0-have-token)))))
              (s1-unknown (()) #f
                          ()
                          (block (if-eos (go s1-eos)
                                         (block get-token
                                                (go s1-have-token)))))
              (s0-have-token (()) #f
                             ()
                             (block (push (state s0))
                                    (token-case (A (push (current-token))
                                                   drop-token
                                                   (go s1-unknown))
                                                (B (go r1-have-token))
                                                (#f (reject)))))
              (s1-have-token (()) #f
                             ()
                             (block (push (state s1))
                                    (token-case (A (push (current-token))
                                                   drop-token
                                                   (go s1-unknown))
                                                (#f (go r1-have-token)))))
              (s0-eos (()) #f
                      ()
                      (block (push (state s0))
                             (reject)))
              (s1-eos (()) #f
                      ()
                      (block (push (state s1))
                             (go r1-eos)))
              (r1-have-token ((A s)) #f
                             ()
                             (block (:= _ (pop))
                                    (:= v (pop))
                                    (:= target (pop))
                                    (semantic-action r1 (v) (ret-val) 'v)
                                    (push target)
                                    (push ret-val)
                                    (state-case target
                                                (s0 (go s1-have-token)))))
              (r2-have-token (()) #f
                             ()
                             (block (:= _ (pop))
                                    (:= v (pop))
                                    (:= target (pop))
                                    (semantic-action r2 (v) (ret-val) 'v)
                                    (push target)
                                    (push ret-val)
                                    (state-case target
                                                (s1 (go s0-have-token)))))
              (r1-eos ((A s)) #f
                      ()
                      (block (:= _ (pop))
                             (:= v (pop))
                             (:= target (pop))
                             (semantic-action r1 (v) (ret-val) 'v)
                             (push target)
                             (push ret-val)
                             (state-case target
                                         (s0 (go s1-eos)))))
              (r2-eos (()) #f
                      ()
                      (block (:= _ (pop))
                             (:= v (pop))
                             (:= target (pop))
                             (semantic-action r2 (v) (ret-val) 'v)
                             (push target)
                             (push ret-val)
                             (state-case target
                                         (s1 (go s0-eos))))))
             (go s0-unknown)))
    ;; (make-pdarisc
    ;;  (list (make-label
    ;;         (list (make-label-polynym #'s0 'unknown)
    ;;               (make-label-polynym #'s1 'unknown)
    ;;               (make-label-polynym #'s0 'have-token)
    ;;               (make-label-polynym #'s1 'have-token)
    ;;               (make-label-polynym #'s0 'eos)
    ;;               (make-label-polynym #'s1 'eos)
    ;;               (make-label-polynym #'r1 'have-token)
    ;;               (make-label-polynym #'r2 'have-token)
    ;;               (make-label-polynym #'r1 'eos)
    ;;               (make-label-polynym #'r2 'eos))
    ;;         `((()) (()) (()) (()) (()) (())
    ;;           ((A s)) (()) ((A s)) (()))
    ;;         '(#f #f #f #f #f #f #f #f #f #f) ; token-regs
    ;;         '(() () () () () () () () () ()) ; arg-lists
    ;;         (list (list (make-block*
    ;;                      (list
    ;;                       (make-if-eos
    ;;                        (make-go (make-label-polynym #'s0 'eos) '())
    ;;                        (make-block*
    ;;                         (list
    ;;                          (make-get-token)
    ;;                          (make-go (make-label-polynym #'s0 'have-token)
    ;;                                   '())))))))
    ;;               (list (make-block*
    ;;                      (list
    ;;                       (make-if-eos
    ;;                        (make-go (make-label-polynym #'s1 'eos) '())
    ;;                        (make-block*
    ;;                         (list
    ;;                          (make-get-token)
    ;;                          (make-go (make-label-polynym #'s1 'have-token)
    ;;                                   '())))))))
    ;;               (list (make-block*
    ;;                      (list (make-push (make-state #'s0))
    ;;                            (make-token-case
    ;;                             (list #'A #'B #f)
    ;;                             (list
    ;;                              (list (make-push (make-curr-token #f))
    ;;                                    (make-drop-token)
    ;;                                    (make-go (make-label-polynym #'s1 'unknown)
    ;;                                             '()))
    ;;                              (list (make-go (make-label-polynym #'r1
    ;;                                                                 'have-token)
    ;;                                             '()))
    ;;                              (list (make-reject)))))))
    ;;               (list (make-block*
    ;;                      (list (make-push (make-state #'s1))
    ;;                            (make-token-case
    ;;                             (list #'A #f)
    ;;                             (list
    ;;                              (list (make-push (make-curr-token #f))
    ;;                                    (make-drop-token)
    ;;                                    (make-go (make-label-polynym #'s1 'unknown)
    ;;                                             '()))
    ;;                              (list (make-go (make-label-polynym #'r1
    ;;                                                                 'have-token)
    ;;                                             '())))))))
    ;;               (list (make-block*
    ;;                      (list (make-push (make-state #'s0))
    ;;                            (make-reject))))
    ;;               (list (make-block*
    ;;                      (list (make-push (make-state #'s1))
    ;;                            (make-go (make-label-polynym #'r1
    ;;                                                         'eos)
    ;;                                     '()))))
    ;;               (list (make-block*
    ;;                      (list
    ;;                       (make-assign (make-nameless-reg) (make-pop))
    ;;                       (make-assign (make-named-reg #'v) (make-pop))
    ;;                       (make-assign (make-named-reg #'target) (make-pop))
    ;;                       (make-sem-act #'r1
    ;;                                     (list (make-named-reg #'v))
    ;;                                     (list (make-named-reg #'ret-val))
    ;;                                     #'v)
    ;;                       (make-push (make-named-reg #'target))
    ;;                       (make-push (make-named-reg #'ret-val))
    ;;                       (make-state-case (make-named-reg #'target)
    ;;                                        (list (make-state #'s0))
    ;;                                        (list
    ;;                                         (list
    ;;                                          (make-go
    ;;                                           (make-label-polynym #'s1
    ;;                                                               'have-token)
    ;;                                           '())))))))
    ;;               (list (make-block*
    ;;                      (list
    ;;                       (make-assign (make-nameless-reg) (make-pop))
    ;;                       (make-assign (make-named-reg #'v) (make-pop))
    ;;                       (make-assign (make-named-reg #'target) (make-pop))
    ;;                       (make-sem-act #'r2
    ;;                                     (list (make-named-reg #'v))
    ;;                                     (list (make-named-reg #'ret-val))
    ;;                                     #'v)
    ;;                       (make-push (make-named-reg #'target))
    ;;                       (make-push (make-named-reg #'ret-val))
    ;;                       (make-state-case (make-named-reg #'target)
    ;;                                        (list (make-state #'s1))
    ;;                                        (list
    ;;                                         (list
    ;;                                          (make-go
    ;;                                           (make-label-polynym #'s0
    ;;                                                               'have-token)
    ;;                                           '())))))))
    ;;               (list (make-block*
    ;;                      (list
    ;;                       (make-assign (make-nameless-reg) (make-pop))
    ;;                       (make-assign (make-named-reg #'v) (make-pop))
    ;;                       (make-assign (make-named-reg #'target) (make-pop))
    ;;                       (make-sem-act #'r1
    ;;                                     (list (make-named-reg #'v))
    ;;                                     (list (make-named-reg #'ret-val))
    ;;                                     #'v)
    ;;                       (make-push (make-named-reg #'target))
    ;;                       (make-push (make-named-reg #'ret-val))
    ;;                       (make-state-case (make-named-reg #'target)
    ;;                                        (list (make-state #'s0))
    ;;                                        (list
    ;;                                         (list
    ;;                                          (make-go
    ;;                                           (make-label-polynym #'s1
    ;;                                                               'eos)
    ;;                                           '())))))))
    ;;               (list (make-block*
    ;;                      (list
    ;;                       (make-assign (make-nameless-reg) (make-pop))
    ;;                       (make-assign (make-named-reg #'v) (make-pop))
    ;;                       (make-assign (make-named-reg #'target) (make-pop))
    ;;                       (make-sem-act #'r2
    ;;                                     (list (make-named-reg #'v))
    ;;                                     (list (make-named-reg #'ret-val))
    ;;                                     #'v)
    ;;                       (make-push (make-named-reg #'target))
    ;;                       (make-push (make-named-reg #'ret-val))
    ;;                       (make-state-case (make-named-reg #'target)
    ;;                                        (list (make-state #'s1))
    ;;                                        (list
    ;;                                         (list
    ;;                                          (make-go
    ;;                                           (make-label-polynym #'s0
    ;;                                                               'eos)
    ;;                                           '()))))))))
    ;;         (list (make-go (make-label-polynym #'s0 'unknown) '())))))
    )))
(require rackunit/text-ui)
(run-tests compile-pda-tests)
#|
(check-syntax-equal? (gather-rto-table
                      (pda-states
                       (parse-pda #'(parse-pda (TOKENS A B plus $eos)
                                               (EOS $eos)
                                               (START s0)
                                               (STATE s0 (())
                                                      (SHIFT (A) s1)
                                                      (REDUCE (B) r1)
                                                      (GOTO nt s1))
                                               (STATE s1 (())
                                                      (SHIFT (A) s1)
                                                      (REDUCE (B) r1)
                                                      (REDUCE () r1)
                                                      (GOTO nt2 s0))
                                               (RULE r1 ((A s)) nt (v) v)
                                               (RULE r2 (()) nt2 (v) v)))))
                     (make-dict 'nt (make-dict #'s0 #'s1)
                                'nt2 (make-dict #'s1 #'s0)))

(define s0
  (first (pda-states simple-pda)))

(define s1
  (second (pda-states simple-pda)))

(check-syntax-equal? (compile-state s0)
                     (make-block*
                      (list
                       (make-if-eos
                        (make-go (make-label-polynym #'s0 'eos) '())
                        (make-block*
                         (list
                          (make-get-token)
                          (make-go (make-label-polynym #'s0 'have-token)
                                   '())))))))

(check-syntax-equal? (compile-have-token-state s0)
                     (make-block*
                      (list
                       (make-push (make-state #'s0))
                       (make-token-case
                        (list #'A #'B #f)
                        (list
                         (list (make-push (make-curr-token #f))
                               (make-drop-token)
                               (make-go (make-label-polynym #'s1 'unknown)
                                        '()))
                         (list (make-go (make-label-polynym #'r1
                                                            'have-token)
                                        '()))
                         (list (make-reject)))))))

(check-syntax-equal? (compile-have-token-state s1)
                     (make-block*
                      (list
                       (make-push (make-state #'s1))
                       (make-token-case
                        (list #'A #f)
                        (list
                         (list (make-push (make-curr-token #f))
                               (make-drop-token)
                               (make-go (make-label-polynym #'s1 'unknown)
                                        '()))
                         (list (make-go (make-label-polynym #'r1
                                                            'have-token)
                                        '())))))))

(check-syntax-equal? (compile-eos-state s0)
                     (make-block*
                      (list
                       (make-push (make-state #'s0))
                       (make-reject))))

(check-syntax-equal? (compile-eos-state s1)
                     (make-block*
                      (list (make-push (make-state #'s1))
                            (make-go (make-label-polynym #'r1 'eos) '()))))

(check-syntax-equal? (compile-rule-args (list #'v1 #f #'v2))
                     (list (make-assign (make-nameless-reg) (make-pop))
                           (make-assign (make-named-reg #'v2) (make-pop))
                           (make-assign (make-nameless-reg)  (make-pop)) ; state
                           (make-assign (make-nameless-reg)  (make-pop))
                           (make-assign (make-nameless-reg)  (make-pop)) ; state
                           (make-assign (make-named-reg #'v1) (make-pop))
                           (make-assign (make-named-reg #'target) (make-pop))))

(check-syntax-equal? (compile-rule-args (list #'v))
                     (list (make-assign (make-nameless-reg) (make-pop))
                           (make-assign (make-named-reg #'v) (make-pop))
                           (make-assign (make-named-reg #'target) (make-pop))))

(check-syntax-equal? (compile-rule-args '())
                     (list (make-assign (make-named-reg #'target) (make-pop))))

(check-syntax-equal? (compile-action (make-shift (list #'A) #'s1) #'s0 #f)
                     (list (make-push (make-curr-token #f))
                           (make-drop-token)
                           (make-go (make-label-polynym #'s1 'unknown) '())))
(check-syntax-equal? (compile-action (make-reduce (list #'B) #'r1) #'s0 #f)
                     (list (make-go (make-label-polynym #'r1 'have-token) '())))
(check-syntax-equal? (compile-action (make-reduce '() #'r1) #'s0 #f)
                     (list (make-go (make-label-polynym #'r1 'have-token) '())))
(check-syntax-equal? (compile-action (make-pda-accept (list #'A)) #'s0 #f)
                     (list (make-assign (make-nameless-reg) (make-pop))
                           (make-assign (make-named-reg #'result) (make-pop))
                           (make-accept (list (make-named-reg #'result)))))

(check-exn exn:fail?
           (lambda ()
             (compile-action (make-shift (list #'A) #'s1) #'s0 #t)))
(check-syntax-equal? (compile-action (make-reduce (list #'B) #'r1) #'s0 #t)
                     (list (make-go (make-label-polynym #'r1 'eos) '())))
(check-syntax-equal? (compile-action (make-reduce '() #'r1) #'s0 #t)
                     (list (make-go (make-label-polynym #'r1 'eos) '())))
(check-syntax-equal? (compile-action (make-pda-accept (list #'A)) #'s0 #t)
                     (list (make-assign (make-nameless-reg) (make-pop))
                           (make-assign (make-named-reg #'result) (make-pop))
                           (make-accept (list (make-named-reg #'result)))))

(check-syntax-equal? (action-has-no-lookahead? (make-pda-accept (list #'A))) #f)
(check-syntax-equal? (action-has-no-lookahead? (make-pda-accept '())) #t)
(check-syntax-equal? (action-has-no-lookahead? (make-goto (list #'A) #'s0)) #f)
(check-syntax-equal? (action-has-no-lookahead? (make-goto '() #'s0)) #t)
(check-syntax-equal? (action-has-no-lookahead?
                      (make-reduce (list #'A) #'r1)) #f)
(check-syntax-equal? (action-has-no-lookahead? (make-reduce '() #'r1)) #t)
(check-syntax-equal? (action-has-no-lookahead? (make-shift (list #'A) #'s0)) #f)
(check-syntax-equal? (action-has-no-lookahead? (make-shift '() #'s0)) #t)
|#