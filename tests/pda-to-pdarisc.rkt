#lang racket
(require rackunit
         "example-risc-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (convert-pda convert-rule convert-state segregate-gotos
                 segregate-eos get-start-state get-eos-token))

;; GET-START-TOKEN and GET-EOS-TOKEN
(check-equal? (get-start-state pda1) 's1)
(check-exn exn:fail?
           (lambda () (get-start-state (cdddr pda1))))
(check-equal? (get-eos-token pda1) '$eos)
(check-exn exn:fail?
           (lambda () (get-eos-token (cddr pda1))))

;; CATEGORIZE-GOTOS
(let-values (((gotos others) (segregate-gotos (cddr multiple-shift-state))))
  (check-equal? gotos '((goto start s4)))
  (check-equal? others '((shift (A) s2) (shift (B) s3))))
(let-values (((gotos others) (segregate-gotos '((shift (F) foo)
                                                (reduce (B) r1)
                                                (reduce ($eos) r3)))))
  (check-equal? gotos '())
  (check-equal? others '((shift (F) foo)
                         (reduce (B) r1)
                         (reduce ($eos) r3))))

;; CATEGORIZE-EOS
(let-values (((eos others) (segregate-eos (cddr accepting-state) '$eos)))
  (check-equal? eos '((accept ($eos))))
  (check-equal? others '()))
(let-values (((eos others) (segregate-eos '((shift (F) foo)
                                            (reduce (B) r1)
                                            (reduce ($eos) r3))
                                          '$eos)))
  (check-equal? eos '((reduce ($eos) r3)))
  (check-equal? others '((shift (F) foo) (reduce (B) r1))))

;; CONVERT-STATE
(for-each (lambda (in expected)
            (check-equal? (convert-state in '$eos) expected))
          (list single-shift-state multiple-shift-state
                reducing-state accepting-state)
          (list single-shift-state-risc multiple-shift-state-risc
                reducing-state-risc accepting-state-risc))

;; CONVERT-RULE
(for-each (lambda (in expected)
            (check-equal? (convert-rule in) expected))
          (list 3-rhs-rule
                2-rhs-rule
                2-rhs-rule-no-bindings)
          (list 3-rhs-rule-risc
                2-rhs-rule-risc
                2-rhs-rule-no-bindings-risc))
