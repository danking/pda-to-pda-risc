#lang racket

(require "search.rkt"
         (only-in "data.rkt" pda-risc-enh-initial-term))
(provide filter-to-list)

(define (filter-to-list pre pred?)
  (folding-forward-search (lambda (t accum)
                            (if (pred? t) (cons t accum) accum))
                          empty
                          (pda-risc-enh-initial-term pre)))
