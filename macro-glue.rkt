#lang racket
(require "parse-pda.rkt" ; so we can reprovide the literals
         (for-syntax "parse-pda.rkt"
                     "compile-pda.rkt"
                     "compile-pdarisc.rkt"))
(provide pda
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
