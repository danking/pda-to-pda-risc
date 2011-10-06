#lang racket
(require "pda-data.rkt")
(provide unparse-pda)

;; unparse-pda : PDA -> SExp
;; unparse-pda produces an s-expression representation of the given PDA.
(define (unparse-pda pda)
  `((TOKENS . ,(pda-tokens pda))
    (EOS ,(pda-eos pda))
    (START ,(pda-start pda))
    ,@(map unparse-rule (pda-rules pda))
    .
    ,(map unparse-state (pda-states pda))))

;; unparse-state : State -> SExp
;; unparse-state produces an s-expression representation of the given State.
(define (unparse-state state)
  `(STATE ,(state-name state)
          :
          ,(state-stack-type state)
          ,@(map unparse-non-goto (state-non-gotos state))
          ,@(map unparse-non-goto (state-eos-actions state))
          .
          ,(map unparse-goto (state-gotos state))))

;; unparse-non-goto : [U Shift Reduce Accept] -> SExp
;; unparse-non-goto produces an s-expression representation of the given Action
;; (this function, obviously, does not handle goto actions)
(define (unparse-non-goto non-goto)
  (match non-goto
    ((shift  lookahead target) `(SHIFT ,lookahead ,target))
    ((reduce lookahead target) `(REDUCE ,lookahead ,target))
    ((accept lookahead)        `(ACCEPT ,lookahead))))

;; unparse-goto : Goto -> SExp
;; unparse-goto produces an s-expression representation of the given Goto
(define (unparse-goto g)
  (match g
    ((goto lookahead target) `(GOTO ,lookahead ,target))))

;; unparse-rule : Rule -> SExp
;; unparse-rule produces an s-expression representation of the given Rule
(define (unparse-rule r)
  (match r
    ((rule name stack-type nt bindings sem-act)
     `(RULE ,name : ,stack-type ,nt ,bindings ,sem-act))))
