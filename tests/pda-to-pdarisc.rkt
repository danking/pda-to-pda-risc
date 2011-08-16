#lang racket
(require rackunit
         "../parse-high-level-pda.rkt"
         "example-risc-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (produce-risc-pda segregate-eos))

(check-equal? (parse-pda pda1)
              pda1-risc-struct)

(check-equal? (produce-risc-pda pda1)
              pda1-risc)

;; CATEGORIZE-EOS
(let-values (((eos others) (segregate-eos (cddr accepting-state) '$eos)))
  (check-equal? eos '((accept $eos)))
  (check-equal? others '()))
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
