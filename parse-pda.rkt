#lang racket
(require syntax/parse
         "pda-syntax-classes.rkt")
(provide parse-pda
         EOS START TOKENS COMMENT STATE RULE
         SHIFT REDUCE ACCEPT)

(define (parse-pda stx)
  (syntax-parse stx
    #:local-conventions ([token identifier])
    #:literal-sets (pda-literals)
    ((_ . pda:untyped-pda-clauses-stx)
     (attribute pda.compiled))
    ((_ . pda:typed-pda-clauses-stx)
     (attribute pda.compiled))))
