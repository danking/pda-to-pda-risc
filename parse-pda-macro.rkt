#lang racket
(require "parse-pda.rkt" ; so we can reprovide the literals
         (for-syntax "parse-pda.rkt"
                     racket/pretty))
(provide (rename-out [parse-pda/macro pda])
         EOS START TOKENS COMMENT STATE RULE
         SHIFT REDUCE ACCEPT)

(define-syntax (parse-pda/macro stx)
  (let ((foo (parse-pda stx)))
    (pretty-print foo)
    #''success))
