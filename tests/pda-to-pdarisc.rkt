#lang racket
(require rackunit
         "../parse-high-level-pda.rkt"
         "example-risc-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (produce-risc-pda))

(check-equal? (parse-pda pda1)
              pda1-risc-struct)

(check-equal? (produce-risc-pda pda1)
              pda1-risc)
