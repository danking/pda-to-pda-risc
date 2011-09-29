#lang racket
(require "pda-data.rkt")
(provide parse-pda)

;; parse-pda : SExp -> PDA
;; parse a sexp representation of a high-level pda into a structure
(define (parse-pda sexp)
  (foldl (lambda (clause pda)
           (match clause
             [(list (or 'STATE 'state) name : stack-type shifts-etc ...)
              (pda-update-states (cons (parse-state/mixed-actions name
                                                                  stack-type
                                                                  shifts-etc)
                                       (pda-states pda))
                                 pda)]
             [(list (or 'RULE 'rule) name : stack-type nt bindings sem-act)
              (pda-update-rules (cons (make-rule name
                                                 stack-type
                                                 nt
                                                 bindings
                                                 sem-act)
                                      (pda-rules pda))
                                pda)]
             [(list (or 'EOS 'eos) token)
              (pda-update-eos token pda)]
             [(list (or 'START 'start) token)
              (pda-update-start token pda)]
             [(list (or 'TOKENS 'tokens) tokens ...)
              (pda-update-tokens tokens pda)]
             [(list (or 'COMMENT 'comment) _ ...)
              pda]
             [else (begin (printf "ignoring unknown pda clause ~a\n" clause)
                          pda)]))
         empty-pda
         sexp))

;; parse-state/mixed-actions : Symbol ST [ListOf SExp] -> PDAState
(define (parse-state/mixed-actions name stack-type actions)
  (let ((actions (filter (lambda (x)
                           (not (or (eq? (car x) 'COMMENT)
                                    (eq? (car x) 'comment))))
                         actions)))
    (let-values
        (((gotos not-gotos)
          (partition (lambda (x)
                       (or (eq? (car x) 'GOTO)
                           (eq? (car x) 'goto)))
                     actions)))
      (make-state name
                  stack-type
                  (map parse-non-goto not-gotos)
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
