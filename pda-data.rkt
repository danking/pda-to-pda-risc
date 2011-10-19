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

;; an ST or StackType is defined in the notes on semantics

;; a State is a (make-state [Syntax Identifier]
;;                          [Maybe ST]
;;                          [ListOf (U Shift
;;                                     Reduce
;;                                     Accept)]
;;                          [ListOf (U Shift
;;                                     Reduce
;;                                     Accept)]
;;                          [ListOf Goto])
(define-ustruct state (name stack-type non-gotos eos-actions gotos))

;; a Rule is a (make-rule Symbol
;;                        ST
;;                        Symbol
;;                        [ListOf [Maybe [Syntax Identifier]]]
;;                        [Syntax Expression])
(define-ustruct rule (name stack-type nt bindings sem-act))


;; an Action is a (make-action Symbol)
(define-ustruct action (lookahead))
;; a TAction is a (make-taction [U [Syntax Identifier] Symbol]
;;                              [Syntax Identifier])
(define-ustruct (taction action) (target))

(define-ustruct (shift   taction) ())
(define-ustruct (reduce  taction) ())
(define-ustruct (goto    taction) ())
(define-ustruct (accept  action) ())
