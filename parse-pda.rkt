#lang racket
(require "pda-data.rkt")
(provide parse-pda
         parse-untyped-pda)

;; find-eos-token : SExp -> Symbol
(define (find-eos-token sexp)
  (let ((eos-form (findf (lambda (clause)
                           (or (eq? (first clause) 'eos)
                               (eq? (first clause) 'EOS)))
                         sexp)))
    (match eos-form
      ((list (or 'EOS 'eos) token) token)
      (else (error 'find-eos-token "No EOS token declared, cannot proceed.")))))

;; parse-pda : SExp -> PDA
;; parse a sexp representation of a high-level pda into a structure
(define (parse-pda sexp)
  (parse-any-pda sexp parse-pda-clause))

;; parse-untyped-pda : SExp -> PDA
;; Parse a sexp representation of a high-level pda which lacks stack type
;; annotations. The stack type annotations are left unspecified in the resulting
;; pda structure
(define (parse-untyped-pda sexp)
  (parse-any-pda sexp parse-untyped-pda-clause))

;; parse-any-pda : SExp (Symbol SExp PDA -> PDA) -> PDA
;; An abstraction over untyped and typed pdas. It handles finding eos and
;; checking for a start state.
(define (parse-any-pda sexp clause-parser)
  (let* ((eos-token (find-eos-token sexp))
         (pda (foldl (curry clause-parser eos-token)
                     (pda-update-eos eos-token empty-pda)
                     sexp)))
    (if (pda-start pda)
        pda
        (error 'parse-pda
               "A PDA form must have a start state. None found in: ~a"
               sexp))))

;; parse-pda-clause : Symbol SExp PDA -> PDA
;; parses one clause of a PDA and adds it to the given pda
(define (parse-pda-clause eos-token clause pda)
  (match clause
    [(list (or 'STATE 'state) name ': stack-type shifts-etc ...)
     (pda-update-states (cons (state-update-stack-type
                               stack-type
                               (parse-state/mixed-actions name
                                                          shifts-etc
                                                          eos-token))
                              (pda-states pda))
                        pda)]
    [(list (or 'RULE 'rule) name ': stack-type nt bindings sem-act)
     (pda-update-rules (cons (make-rule name
                                        stack-type
                                        nt
                                        bindings
                                        sem-act)
                             (pda-rules pda))
                       pda)]
    [(list (or 'EOS 'eos) token) pda]
    [(list (or 'START 'start) token)
     (pda-update-start token pda)]
    [(list (or 'TOKENS 'tokens) tokens ...)
     (pda-update-tokens tokens pda)]
    [(list (or 'COMMENT 'comment) _ ...)
     pda]
    [else (begin (printf "ignoring unknown pda clause ~a\n" clause)
                 pda)]))

(define (parse-untyped-pda-clause eos-token clause pda)
  (match clause
    [(list (or 'STATE 'state) name shifts-etc ...)
     (pda-update-states (cons (parse-state/mixed-actions
                               name
                               shifts-etc
                               eos-token)
                              (pda-states pda))
                        pda)]
    [(list (or 'RULE 'rule) name nt bindings sem-act)
     (pda-update-rules (cons (make-rule name
                                        #f
                                        nt
                                        bindings
                                        sem-act)
                             (pda-rules pda))
                       pda)]
    [else (parse-pda-clause eos-token clause pda)]))

;; parse-state/mixed-actions : Symbol ST [ListOf SExp] Symbol -> PDAState
;; Parses a state's list of actions into a series of structures which are then
;; placed into a state structure.
;; This procedure IGNORES stack type. It sets the stack type of the produced
;; state to #f.
(define (parse-state/mixed-actions name actions eos-token)
  (let ((actions (filter (lambda (x)
                           (not (or (eq? (car x) 'COMMENT)
                                    (eq? (car x) 'comment))))
                         actions)))
    (let*-values
        (((gotos not-gotos)
          (partition (lambda (x)
                       (or (eq? (car x) 'GOTO)
                           (eq? (car x) 'goto)))
                     actions))
         ((eos not-eos)
          (partition (lambda (x)
                       (match x
                         ((list type (list lookahead) etc ...)
                          (eq? lookahead eos-token))
                         (else #f)))
                     not-gotos)))
      (make-state name
                  #f
                  (map parse-non-goto not-eos)
                  (map parse-non-goto eos)
                  (map parse-goto gotos)))))

(define (parse-goto g)
  (make-goto (second g) (third g)))

(define (parse-non-goto a)
  (match a
    ((list (or 'shift 'SHIFT) lookahead target)
     (make-shift lookahead target))
    ((list (or 'reduce 'REDUCE) lookahead target)
     (make-reduce lookahead target))
    ((list (or 'accept 'ACCEPT) lookahead)
     (make-accept lookahead))))
