#lang racket
(require (except-in "../pdarisc-data.rkt"
                    register
                    struct:register
                    make-register
                    register?

                    named-reg
                    struct:named-reg
                    make-named-reg
                    named-reg?
                    named-reg-id

                    nameless-reg
                    struct:nameless-reg
                    make-nameless-reg
                    nameless-reg?

                    label-name
                    struct:label-name
                    make-label-name
                    label-name?
                    label-name-id

                    label-polynym
                    struct:label-polynym
                    make-label-polynym
                    label-polynym?
<<<<<<< HEAD
                    label-polynym-extra-id

                    label-name->symbol)
=======
                    label-polynym-extra-id)
>>>>>>> unparse
         (prefix-in old:
                    (only-in "../pdarisc-data.rkt"
                             register
                             struct:register
                             make-register
                             register?

                             named-reg
                             struct:named-reg
                             make-named-reg
                             named-reg?
                             named-reg-id

                             nameless-reg
                             struct:nameless-reg
                             make-nameless-reg
                             nameless-reg?

                             label-name
                             struct:label-name
                             make-label-name
                             label-name?
                             label-name-id

                             label-polynym
                             struct:label-polynym
                             make-label-polynym
                             label-polynym?
                             label-polynym-extra-id

                             label-name->symbol)))
(provide (all-defined-out)
         ;; Re-exports
         (all-from-out "../pdarisc-data.rkt"))

;; a PDA-RISC-ENH term is a:
;;   (make-pdarisc Term*-Seq)

;; a Term-Seq is a [ListOf Term]
;; a Term*-Seq is a (append Term-Seq (list Term*))

;; a Term is a (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       Register
;;                       [SetEq Register]
;;                       Insn)

;; a Term* is the same as a Term except it contains an Insn instead of an Insn*
;; concretely, (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       Register
;;                       [SetEq Register]
;;                       Insn*)

;; Insn and Insn* are defined in ../pdarisc-data.rkt
;; Except:
;;   - instances of Register are replaced with the register struct defined
;;     in this file, and all
;;   - instances of LabelName are replaced with the label-name struct in this
;;     file
;;   - every labeled insn-seq* in a label form has a join-point appended to it
;;     which records the label and argument list associated with that insn-seq*

(define (uninitialized-pda-term insn)
  (pda-term (seteq) (seteq)
            #f #f
            insn))

(struct pda-term (preds
                  succs
                  avail-regs
                  live-regs
                  insn)
        #:mutable
        #:transparent
        #:property prop:equal+hash (list (lambda (x y recur-equal?)
                                           (match (list x y)
                                             ((list (pda-term _ _ _ _ i1)
                                                    (pda-term _ _ _ _ i2))
                                              (recur-equal? i1 i2))))
                                         (lambda (x _)
                                           (eq-hash-code x))
                                         (lambda (x _)
                                           (- (eq-hash-code x)))))

(define (pda-risc-enh-initial-term pre)
  (match pre
    ((pdarisc seq) (first seq))))

(define (uninitialized-register name)
  (register name #f #f (seteq)))
(define (register-equal? x y)
  (eq? (register-uid x) (register-uid y)))
(define (register-hash-code x)
  (register-uid x))
(define (register-secondary-hash-code x)
  (- (register-uid x)))
(struct register (lexical-name
                  uid
                  [binding #:mutable]
                  [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (register-equal? x y))
                                         (lambda (x _) (register-hash-code x))
                                         (lambda (x _) (register-secondary-hash-code x))))
(define (register-add-use! r u)
  (set-register-uses! r (set-add (register-uses r) u)))

(define (uninitialized-label-name name)
  (label-name name #f #f (seteq)))
(define (label-name-equal? x y)
  (eq? (label-name-uid x) (label-name-uid y)))
(define (label-name-hash-code x)
  (label-name-uid x))
(define (label-name-secondary-hash-code x)
  (- (label-name-uid x)))
(struct label-name (lexical-name
                    uid
                    [binding #:mutable]
                    [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (label-name-equal? x y))
                                         (lambda (x _) (label-name-hash-code x))
                                         (lambda (x _) (label-name-secondary-hash-code x))))
(define (label-name-add-use! r u)
  (set-label-name-uses! r (set-add (label-name-uses r) u)))

;; a join-point is an insn
(struct join-point (label params) #:transparent)

(define (raise-to-term f)
  (lambda (t . rest)
    (match t
      ((pda-term a b c d i)
       (pda-term a b c d (apply f (cons i rest)))))))

(define (raise-input-to-term f)
  (lambda (t . rest)
    (match t
      ((pda-term a b c d i)
       (apply f (cons i rest))))))

;; A pda-term is
;;
;;   < preds, succs, avail-regs, live-regs, insn >
;;
;; preds ⊆ P(pda-term)
;; succs ⊆ P(pda-term)
;; avail-regs ⊆ P(registers)
;; live-regs ⊆ P(registers)
;; insn ⊆ Insn ∪ Insn*

;; ** All four fields should be mutable sets.
;;
;; The preds field is a set of all instructions which could potentially *precede*
;; this term in the dynamic execution of the PDA.
;;
;; The succs field is a set of all instructions which could potentially *succeed*
;; this term in the dynamic execution of the PDA
;;
;; The avail-regs field is a set of all registers which are in-scope at this term
;;
;; The live-regs field is a set of all registers which could potentially be used by
;; a term in the transitive closure of succs.


;; A register is
;;
;;   < lexical-name, uid, binding, uses >
;;
;; lexical-name ⊆ Symbol
;; uid ⊆ Number
;; binding ⊆ pda-term
;; uses ⊆ P(pda-term)
;;
;; The lexical-name field is a Syntax object from the input text
;;
;; The uid field is a number which is unique amongst all registers in this PDA
;; term.
;;
;; The binding field is the pda-term which binds this register
;;
;; The uses field is a set of pda-terms which reference this register
