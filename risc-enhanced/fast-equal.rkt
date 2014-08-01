#lang racket

(require "data.rkt"
         "basic-block-data.rkt")
(provide fast-term-equal?
         fast-block-equal?)

(define (fast-term-equal? x y)
  (= (pda-term->uid x) (pda-term->uid y)))

(define (fast-block-equal? x y)
  (= (block-uid x) (block-uid y)))
