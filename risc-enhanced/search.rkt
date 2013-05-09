#lang racket

(require "../../racket-utils/workset-loop.rkt"
         "data.rkt")
(provide folding-search)

;; folding-search : [Term X -> X] X Term -> X
(define (folding-search f b t)
  (let loop ((W (seteq t))
             (seen (seteq))
             (b b))
    (if (set-empty? W) b
        (let ((t (set-first W))
              (W (set-rest W)))
          (let* ((new-succs (set-subtract (pda-term-succs t) seen))
                 (new-W (set-union new-succs W))
                 (new-seen (set-union new-succs seen)))
            (loop new-W new-seen (f t b)))))))
