#lang racket
(require rackunit
         "../parse-high-level-pda.rkt"
         "../pda-data.rkt"
         "example-risc-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (produce-risc-pda translate-rule translate-state))

(check-equal? (parse-pda pda1)
              pda1-risc-struct)

(check-equal? (produce-risc-pda pda1)
              pda1-risc)


(define calculator-pda-struct (parse-pda calculator-pda))

(check-equal? (translate-rule calculator-pda-struct
                              (make-rule 'r1 '*start 2 #f))
              '((r1
                 ()
                 (:= v1 (pop))
                 (:= st1 (pop))
                 (:= v2 (pop))
                 (:= st2 (pop))
                 (semantic-action (v1 v2) (result) #f)
                 (push st2)
                 (push result)
                 (state-case st2))
                (r1-eos
                 ()
                 (:= v1 (pop))
                 (:= st1 (pop))
                 (:= v2 (pop))
                 (:= st2 (pop))
                 (semantic-action (v1 v2) (result) #f)
                 (push st2)
                 (push result)
                 (state-case st2))))

(check-equal? (translate-rule calculator-pda-struct
                              (make-rule 'r2 'program 1 'v1))
              '((r2
                 ()
                 (:= v1 (pop))
                 (:= st1 (pop))
                 (semantic-action (v1) (result) v1)
                 (push st1)
                 (push result)
                 (state-case st1
                             (s0 (go s1-have-token))))
                (r2-eos
                 ()
                 (:= v1 (pop))
                 (:= st1 (pop))
                 (semantic-action (v1) (result) v1)
                 (push st1)
                 (push result)
                 (state-case st1
                             (s0 (go s1-eos))))))

(check-equal? (translate-rule calculator-pda-struct
                              (make-rule 'r5 's-list 0 '()))
              '((r5
                 ()
                 (semantic-action () (result) ())
                 (:= v (pop))
                 (:= st (pop))
                 (push st)
                 (push v)
                 (push st)
                 (push result)
                 (state-case st
                             (s0 (go s2-have-token))))
                (r5-eos
                 ()
                 (semantic-action () (result) ())
                 (:= v (pop))
                 (:= st (pop))
                 (push st)
                 (push v)
                 (push st)
                 (push result)
                 (state-case st
                             (s0 (go s2-eos))))))

(check-equal? (translate-state (make-state
                                's1
                                (list (make-accept '*EOF* #f))
                                (list))
                               '*EOF*)
              '((s1 ()
                    (if-eos (go s1-eos)
                            (block get-token
                                   (go s1-have-token))))
                (s1-eos ()
                        (block (:= final-semantic-value (pop))
                               (accept final-semantic-value)))
                (s1-have-token ()
                               (token-case))))

(check-equal? (translate-state (make-state
                                's2
                                (list (make-shift 'NUM 's5)
                                      (make-shift 'L-PAREN 's6)
                                      (make-shift '*ERROR* 's7)
                                      (make-reduce '*EOF* 'r2))
                                (list (make-goto 'statement 's3)
                                      (make-goto 'exp 's4)))
                               '*EOF*)
              '((s2 ()
                    (if-eos (go s2-eos)
                            (block get-token
                                   (go s2-have-token))))
                (s2-eos ()
                        (go r2-eos))
                (s2-have-token ()
                               (token-case
                                (NUM (block (push (state s2))
                                            (push (current-token))
                                            drop-token
                                            (go s5)))
                                (L-PAREN (block (push (state s2))
                                                (push (current-token))
                                                drop-token
                                                (go s6)))
                                (*ERROR* (block (push (state s2))
                                                (push (current-token))
                                                drop-token
                                                (go s7)))))))

