#lang racket
(provide (all-defined-out))
(require "define-ustruct.rkt")

;; A PDA is a (make-pda [ListOf Symbol]
;;                      [Maybe Symbol]
;;                      [Maybe Symbol]
;;                      [ListOf State]
;;                      [ListOf Rule])
(define-ustruct pda (tokens eos start states rules))

(define empty-pda (make-pda '() #f #f '() '()))

;; a State is a (make-state Symbol
;;                          [ListOf (U Shift
;;                                     Reduce
;;                                     Accept)]
;;                          Goto)
(define-ustruct state (name non-gotos gotos))

;; a Rule is a (make-rule Symbol
;;                        Symbol
;;                        [ListOf [Maybe Symbol]]
;;                        SExp)
(define-ustruct rule (name nt bindings sem-act))


;; an Action is a (make-action Symbol)
(define-ustruct action (lookahead))
;; a TAction is a (make-taction Symbol Symbol)
(define-ustruct (taction action) (target))

(define-ustruct (shift   taction) ())
(define-ustruct (reduce  taction) ())
(define-ustruct (goto    taction) ())
(define-ustruct (accept  action) ())
