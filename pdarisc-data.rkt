#lang racket
(provide (all-defined-out))
(require "define-ustruct.rkt")

;; a PDA-RISC is a (make-pdarisc Insn*-Seq)
(define-ustruct pdarisc (insns))

;; an Insn is one of
;;  - (make-assign RegName Var-Rhs)
;;  - (make-push Pure-Rhs)
;;  - (make-sem-act [Syntax Identifier]
;;                  [ListOf RegName]
;;                  [ListOf [Maybe RegName]]
;;                  SExp)
;;  - (make-drop-token)
;;  - (make-get-token)
;;  - (make-stack-ensure Natural)
;;  - (make-block [ListOf Insn])
(define-ustruct insn ())
(define-ustruct (assign insn) (id val))
(define-ustruct (push insn) (val))
(define-ustruct (sem-act insn) (name params retvars action))
(define-ustruct (drop-token insn) ())
(define-ustruct (get-token insn) ())
(define-ustruct (stack-ensure insn) (hdrm))
(define-ustruct (block insn) (insns))

;; an Insn* is one of
;;  - (make-label [ListOf LabelName]
;;                [ListOf ST] ;; ST is defined in pda-data.rkt
;;                TokenType ;; TokenType is defined in the semantics notes
;;                [ListOf ArgList]
;;                [ListOf Insn*-Seq]
;;                Insn*-Seq)
;;  - (make-block Insn*-Seq)
;;
;; an Insn*-Seq is a (append [ListOf Insn] (list Insn*))
;; an ArgList is a [ListOf RegName]
(define-ustruct insn* ())
(define-ustruct (label insn*) (ids stack-types token-types
                                   param-lists bodies body))
(define-ustruct (block* insn*) (insns))


;; a Branch is one of
;;  - (make-accept [ListOf RegName])
;;  - (make-reject)
;;  - (make-if-eos Insn* Insn*)
;;  - (make-state-case RegName
;;                     [ListOf [Syntax Identifier]]
;;                     [ListOf Insn*-Seq])
;;  - (make-token-case [ListOf [Maybe Token]] [ListOf Insn*-Seq])
;;  - (make-go LabelName [ListOf Pure-Rhs])
(define-ustruct (branch insn*) ())
(define-ustruct (accept branch) (vals))
(define-ustruct (reject branch) ())
(define-ustruct (if-eos branch) (cnsq altr))
(define-ustruct (state-case branch) (st lookaheads cnsqs))
(define-ustruct (token-case branch) (lookaheads cnsqs))
(define-ustruct (go branch) (target args))

;; A Var-Rhs is one of
;;  - (make-pop)
;;  - Pure-Rhs
(define-ustruct var-rhs ())
(define-ustruct (pop var-rhs) ())

;; A Pure-Rhs is one of
;;  - Register
;;  - (make-state [Syntax Identifier])
;;  - (make-nterm Symbol)
;;  - (make-curr-token [Maybe Natural])
(define-ustruct (pure-rhs var-rhs) ())
(define-ustruct (state pure-rhs) (id))
(define-ustruct (nterm pure-rhs) (id))
(define-ustruct (curr-token pure-rhs) (n))

;; A Register is either
;;  - a (make-named-reg [Syntax Idnetifier]), or
;;  - a (make-nameless-reg)
(define-ustruct (register pure-rhs) ())
(define-ustruct (named-reg register) (id))
(define-ustruct (nameless-reg register) ())

;; A LabelName is either
;;  - a (make-label-name [Syntax Identifier]), or
;;  - a (make-label-polynym [Syntax Identifier] Symbol)
(define-ustruct label-name (id))
(define-ustruct (label-polynym label-name) (extra-id))

;; A Token is [Syntax Identifier]
