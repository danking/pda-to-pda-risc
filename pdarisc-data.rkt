#lang racket
(provide (all-defined-out))
(require "define-ustruct.rkt")

;; a PDA-RISC is a (make-pdarisc Natural Insn*-Seq)
;; the natural number is the last used uid
(define-ustruct pdarisc (max-uid insns))

;; an Insn is one of
;;  - (make-assign RegName Var-Rhs)
;;  - (make-push Pure-Rhs)
;;  - (make-sem-act [Syntax Identifier]
;;                  [ListOf RegName]
;;                  [ListOf [Maybe RegName]]
;;                  [Syntax SExp])
;;  - (make-drop-token)
;;  - (make-get-token)
;;  - (make-stack-ensure Natural)
;;  - (make-block [ListOf Insn])
(define-ustruct insn (uid))
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
;;  - Branch
;;
;; an Insn*-Seq is a (append [ListOf Insn] (list Insn*))
;; an ArgList is a [ListOf RegName]
(define-ustruct insn* (uid))
(define-ustruct (label insn*) (ids stack-types token-types
                                   param-lists bodies body)
  #:mutable) ;; needed for risc enhanced code
(define-ustruct (block* insn*) (insns))


;; a Branch is one of
;;  - (make-accept [ListOf RegName])
;;  - (make-reject)
;;  - (make-if-eos Insn* Insn*)
;;  - (make-state-case RegName
;;                     [ListOf (make-state [Syntax Identifier])]
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
;;  - a (make-named-reg Symbol), or
;;  - a (make-nameless-reg)
(define-ustruct (register pure-rhs) ())
(define-ustruct (named-reg register) (id))
(define-ustruct (nameless-reg register) ())

;; A LabelName is either
;;  - a (make-label-name Symbol), or
(define-ustruct label-name (id))

;; A Token is [Syntax Identifier]

;; A RegName is a Register

;; pda-equal?/no-uid : PDA-RISC PDA-RISC -> Boolean
(define (pda-equal?/no-uid x y)
  (insn-seq-equal?/no-uid (pdarisc-insns x)
                          (pdarisc-insns y)))

;; insn-seq-equal?/no-uid : Insn-Seq* Insn-Seq* -> Boolean
(define (insn-seq-equal?/no-uid xs ys)
  (and (= (length xs)
          (length ys))
       (for/and ([x xs] [y ys])
         (insn-equal?/no-uid x y))))

;; insn-equal?/no-uid : [U Insn Insn*] [U Insn Insn*] -> Boolean
(define (insn-equal?/no-uid x y)
  (match* (x y)
    (((assign _ id val) (assign _ id2 val2))
     (and (equal? id id2)
          (equal? val val2)))
    (((push _ val) (push _ val2))
     (equal? val val2))
    (((sem-act _ name params retvars action)
      (sem-act _ name2 params2 retvars2 action2))
     (and (equal? name name2)
          (equal? params params2)
          (equal? retvars retvars2)
          (equal? action action2)))
    (((drop-token _) (drop-token _)) #t)
    (((get-token _) (get-token _)) #t)
    (((stack-ensure _ hdrm) (stack-ensure _ hdrm2))
     (equal? hdrm hdrm2))
    (((block _ insns) (block _ insns2))
     (insn-seq-equal?/no-uid insns insns2))
    (((label _ ids stack-types token-types param-lists bodies body)
      (label _ ids2 stack-types2 token-types2 param-lists2 bodies2 body2))
     (and (equal? ids ids2)
          (equal? stack-types stack-types2)
          (equal? token-types token-types2)
          (equal? param-lists param-lists2)
          (andmap insn-seq-equal?/no-uid bodies bodies2)
          (insn-seq-equal?/no-uid body body2)))
    (((block* _ insns) (block* _ insns2))
     (insn-seq-equal?/no-uid insns insns2))
    (((accept _ vals) (accept _ vals2))
     (equal? vals vals2))
    (((reject _) (reject _)) #t)
    (((if-eos _ cnsq altr) (if-eos _ cnsq2 altr2))
     (and (insn-equal?/no-uid cnsq cnsq2)
          (insn-equal?/no-uid altr altr2)))
    (((state-case _ st lookaheads cnsqs) (state-case _ st2 lookaheads2 cnsqs2))
     (and (equal? st st2)
          (equal? lookaheads lookaheads2)
          (andmap insn-seq-equal?/no-uid cnsqs cnsqs2)))
    (((token-case _ lookaheads cnsqs) (token-case _ lookaheads2 cnsqs2))
     (and (equal? lookaheads lookaheads2)
          (andmap insn-seq-equal?/no-uid cnsqs cnsqs2)))
    (((go _ target args) (go _ target2 args2))
     (and (equal? target target2)
          (equal? args args2)))
    ((_ _) #f)))
