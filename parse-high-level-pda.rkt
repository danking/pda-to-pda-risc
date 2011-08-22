#lang racket
(require "pda-data.rkt")
(provide parse-pda)

;; parse a sexp representation of a high-level pda into a structure
(define (parse-pda sexp)
  (foldl (lambda (clause pda)
           (match clause
             [(list (or 'STATE 'state) name shifts-etc ...)
              (pda-add-state (make-state/mixed-actions name shifts-etc) pda)]
             [(list (or 'RULE 'rule) name nt bindings sem-act)
              (pda-add-rule (make-rule name nt bindings sem-act) pda)]
             [(list (or 'EOS 'eos) token)
              (pda-set-eos token pda)]
             [(list (or 'START 'start) token)
              (pda-set-start token pda)]
             [(list (or 'TOKENS 'tokens) tokens ...)
              (pda-set-tokens tokens pda)]
             [(list (or 'COMMENT 'comment) _ ...)
              pda]
             [else (begin (printf "ignoring unknown pda clause ~a\n" clause)
                          pda)]))
         empty-pda
         sexp))

;; make-state/mixed-actions : Symbol [ListOf StateAction] -> PDAState
(define (make-state/mixed-actions name shifts-and-other-actions)
  (let-values
      (((gotos not-gotos)
        (partition (lambda (x)
                     (or (eq? (car x) 'GOTO)
                         (eq? (car x) 'goto)))
                   shifts-and-other-actions)))
    (make-state name not-gotos gotos)))
