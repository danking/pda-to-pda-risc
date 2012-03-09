#lang racket
(provide symbol-append)

(define-syntax symbol-append
  (syntax-rules ()
    [(_ sym ...)
     (string->symbol (string-append (symbol->string sym) ...))]))