#lang racket
(require "traverse-pdarisc.rkt")
(provide strip-syntax strip-insn strip-insn*)

(define-values
  (strip-syntax strip-insn strip-insn*)
  (traverse-pdarisc #:syntax strip))

(define (strip stx)
  (syntax->datum stx))
