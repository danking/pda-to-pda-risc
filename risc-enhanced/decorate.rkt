#lang racket
(require "from-risc.rkt"
         "set-uids.rkt"
         "set-preds.rkt"
         "set-avail-and-live-regs.rkt")
(provide decorate)

;; decorate: PDA-RISC -> PDA-RISC-ENH
;; PDA-RISC-ENH is not yet defined anywhere, but essentially, every insn is
;; wrapped in a pda-term structure, every register is replaced with a more
;; expressive register structure, every label is replaced with a more
;; expressive label-name structure, and join-points are inserted at the
;; beginning of labeled insn-seq*s. These four new structures orginate in
;; data.rkt
(define (decorate pr)
  (let* ((pre (convert/pdarisc pr))
         (pre (set-uids pre)))
    (set-preds! pre)
    (set-avail/live-regs! pre)
    pre))