#lang racket
(require syntax/parse
         "pda-syntax-classes.rkt"
         "pda-parse-post-processing.rkt"
         "infer-pda-types.rkt")
(provide parse-pda
         EOS START TOKENS COMMENT STATE RULE
         SHIFT REDUCE ACCEPT)

(define (parse-pda stx)
  (post-process
   (syntax-parse stx
     #:local-conventions ([token identifier])
     #:literal-sets (pda-literals)
     ((_ . pda:untyped-pda-clauses-stx)
      (infer-pda-types (attribute pda.compiled)))
     ((_ . pda:typed-pda-clauses-stx)
      (attribute pda.compiled)))))
