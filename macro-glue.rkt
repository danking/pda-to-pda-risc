#lang racket
(require "parse-pda.rkt" ; so we can reprovide the literals
         (for-syntax "parse-pda.rkt"
                     "compile-pda.rkt"
                     "compile-pdarisc.rkt")
         "parse-pda.rkt"
         "compile-pda.rkt"
         "compile-pdarisc.rkt")
(provide pda pda->pda-risc pda->pda-risc/no-syntax
         EOS START TOKENS COMMENT STATE RULE
         SHIFT REDUCE ACCEPT)

(define-syntax (pda stx)
  (syntax-case stx ()
    ((pda token-convert get-token drop-token eos-stream? (clauses ...))
     (compile-pdarisc (compile-pda (parse-pda #'(pda clauses ...)))
                      #'token-convert
                      #'get-token
                      #'drop-token
                      #'eos-stream?))))

(define-syntax (pda->pda-risc stx)
  (syntax-case stx ()
    ((pda clauses ...)
     #'(compile-pda (parse-pda #'(pda clauses ...))))))

(define-syntax (pda->pda-risc/no-syntax stx)
  (syntax-case stx ()
    ((pda clauses ...)
     #'(remove-syntax (compile-pda (parse-pda #'(pda clauses ...)))))))


(define (remove-syntax sexp)
  (cond
   [(list? sexp)
    (map remove-syntax sexp)]
   [(struct? sexp)
    (map remove-syntax (vector->list (struct->vector sexp)))]
   [(syntax? sexp)
    (remove-syntax (syntax->datum sexp))]
   [else sexp]))
