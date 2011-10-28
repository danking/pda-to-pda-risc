#lang racket
(require syntax/parse)
(provide (all-defined-out))

(define-syntax define-literals
  (syntax-rules ()
    ((_ (literal ...))
     (begin (define literal
              (lambda (stx)
                (raise-syntax-error 'parse-pda
                                    "~a should only be used inside parse-pda"
                                    stx)))
            ...))))

(define-literals (EOS START TOKENS COMMENT STATE RULE))

(define-literals (SHIFT REDUCE ACCEPT))
