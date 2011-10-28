#lang racket
(require "pda-data.rkt")
(provide post-process)

;; post-process : PDA -> PDA
;; cleans up the PDA in the following ways:
;;  - segregates eos actions from non-eos actions in States
(define (post-process a-pda)
  (match a-pda
    ((pda tokens eos start states rules)
     (make-pda tokens
               eos
               start
               (map (curry segregate-eos-actions eos) states)
               rules))))

;; segregate-eos-actions : [Syntax Identifier] State -> State
(define (segregate-eos-actions eos-token st)
  (match st
    ((state name stack-type other-actions _ gotos)
     (let-values (((eos-actions non-eos-actions)
                   (partition (lambda (x)
                                (match x
                                  ((action (list lookahead))
                                   (free-identifier=? lookahead eos-token))
                                  (else #f)))
                              other-actions)))
       (make-state name
                   stack-type
                   non-eos-actions
                   eos-actions
                   gotos)))))
