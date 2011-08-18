#lang racket
(provide (all-defined-out))

;; a PDAState is a (make-state Symbol
;;                             [ListOf (U ShiftAction
;;                                        ReduceAction
;;                                        AcceptAction)]
;;                             GotoAction)
(define-struct state (name non-gotos gotos)
  #:transparent)

;; an [Action X] is a (list X lookahead target)
;; where lookahead is a Symbol, and
;;       target is a Symbol

;; a ShiftAction is an [Action 'shift]

;; a ReduceAction is an [Action 'reduce]

;; a GotoAction is an [Action 'goto]

;; an AcceptAction is a (list 'accept lookahead)
;; where lookahead is a Symbol

;; StateAction is (U ShiftAction ReduceAction GotoAction AcceptAction)

(define-struct pda (tokens eos start states rules reducible-states)
  #:transparent)
(define-struct rule (name nt bindings sem-act)
  #:transparent)

(define empty-pda (make-pda '() #f #f '() '() (hasheq)))

(define (pda-set-reducible-states hsh pda)
  (make-pda (pda-tokens pda)
            (pda-eos pda)
            (pda-start pda)
            (pda-states pda)
            (pda-rules pda)
            hsh))

(define (pda-set-tokens tokens pda)
  (make-pda tokens
            (pda-eos pda)
            (pda-start pda)
            (pda-states pda)
            (pda-rules pda)
            (pda-reducible-states pda)))

(define (pda-set-eos token pda)
  (make-pda (pda-tokens pda)
            token
            (pda-start pda)
            (pda-states pda)
            (pda-rules pda)
            (pda-reducible-states pda)))

(define (pda-set-start state pda)
  (make-pda (pda-tokens pda)
            (pda-eos pda)
            state
            (pda-states pda)
            (pda-rules pda)
            (pda-reducible-states pda)))

(define (pda-add-state state pda)
  (make-pda (pda-tokens pda)
            (pda-eos pda)
            (pda-start pda)
            (cons state (pda-states pda))
            (pda-rules pda)
            (pda-reducible-states pda)))

(define (pda-add-rule rule pda)
  (make-pda (pda-tokens pda)
            (pda-eos pda)
            (pda-start pda)
            (pda-states pda)
            (cons rule (pda-rules pda))
            (pda-reducible-states pda)))
