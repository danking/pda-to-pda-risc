#lang racket
(require rackunit
         "../parse-high-level-pda.rkt"
         "example-risc-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (produce-risc-pda segregate-eos
                 make-risc-rules make-risc-states))

(check-equal? (parse-pda pda1)
              pda1-risc-struct)

(check-equal? (produce-risc-pda pda1)
              pda1-risc)

(check-equal? (make-risc-states single-shift-state '$eos)
              single-shift-state-risc)

(for-each (lambda (arg expected)
            (check-equal? (make-risc-states arg '$eos)
                          expected))
          (list single-shift-state multiple-shift-state
                reducing-state accepting-state)
          (list single-shift-state-risc multiple-shift-state-risc
                reducing-state-risc accepting-state-risc))

(for-each (lambda (arg expected)
            (check-equal? (make-risc-rules arg
                                           rule-state-hash)
                          expected))
          (list 3-rhs-rule 2-rhs-rule 2-rhs-rule-no-bindings)
          (list 3-rhs-rule-risc 2-rhs-rule-risc 2-rhs-rule-no-bindings-risc))

;; CATEGORIZE-EOS
(let-values (((eos others) (segregate-eos '((accept $eos)
                                            (reduce B r1)
                                            (shift D r3))
                                          '$eos)))
  (check-equal? eos '((accept $eos)))
  (check-equal? others '((reduce B r1) (shift D r3))))
(let-values (((eos others) (segregate-eos '((shift F foo)
                                            (reduce B r1)
                                            (reduce $eos r3))
                                          '$eos)))
  (check-equal? eos '((reduce $eos r3)))
  (check-equal? others '((shift F foo) (reduce B r1))))

;; CONVERT-STATE
;; (for-each (lambda (in expected)
;;             (check-equal? (convert-state in '$eos) expected))
;;           (list single-shift-state multiple-shift-state
;;                 reducing-state accepting-state)
;;           (list single-shift-state-risc multiple-shift-state-risc
;;                 reducing-state-risc accepting-state-risc))

;; CONVERT-RULE
;; (for-each (lambda (in expected)
;;             (check-equal? (convert-rule in) expected))
;;           (list 3-rhs-rule
;;                 2-rhs-rule
;;                 2-rhs-rule-no-bindings)
;;           (list 3-rhs-rule-risc
;;                 2-rhs-rule-risc
;;                 2-rhs-rule-no-bindings-risc))
