#lang racket

(require "../../racket-utils/workset-loop.rkt"
         "data.rkt")
(provide folding-forward-search
         folding-backwards-search)

;; folding-forward-search : [Term X -> X] X Term -> X
(define (folding-forward-search f b t)
  (folding-search f b t pda-term-succs))

;; folding-backwards-search : [Term X -> X] X Term -> X
(define (folding-backwards-search f b t)
  (folding-search f b t pda-term-preds))

(define (folding-search f b t next-terms-to-look-at)
  (let loop ((W (seteq t))
             (seen (seteq))
             (b b))
    (if (set-empty? W) b
        (let ((t (set-first W))
              (W (set-rest W)))
          (let* ((new-succs (set-subtract (next-terms-to-look-at t) seen))
                 (new-W (set-union new-succs W))
                 (new-seen (set-union new-succs seen)))
            (loop new-W new-seen (f t b)))))))
