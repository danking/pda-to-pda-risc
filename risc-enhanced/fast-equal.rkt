#lang racket

(require "data.rkt")
(provide fast-term-equal?)

(define (fast-term-equal? x y)
  (= (pda-term->uid x) (pda-term->uid y)))
