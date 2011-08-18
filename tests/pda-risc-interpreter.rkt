#lang racket
(require rackunit
         "example-risc-pdas.rkt"
         "../pda-risc-interpreter.rkt")

(check-equal? (car (start pda1-risc '(A B)))
              2)
(check-equal? (car (start pda1-risc '(A A A B B B)))
              3)
(check-equal? (car (start pda1-risc '(A A A A A A A B B B B B B B)))
              7)
(check-exn (lambda (e)
             (and (exn:fail? e)
                  (regexp-match? #rx"unexpected end of stream"
                                 (exn-message e))))
           (lambda () (start pda1-risc '(A A B))))
