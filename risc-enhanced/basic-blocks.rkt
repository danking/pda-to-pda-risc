#lang racket

(provide (all-defined-out))
(require "data.rkt"
         "basic-block-data.rkt"
         "uid.rkt"
         racket/contract/region)

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build ebb

(define-values
  (next-uid current-uid reset-uid set-uid)
  (init))

(define (create-basic-blocks pre)
  (define initial-term (pda-risc-enh-initial-term pre))
  (set-uid (pdarisc-max-uid pre))

  (define (loop W Seen)
    (cond
     [(set-empty? W)
      (void)]
     [else
      (match-define (list parent-block new-term) (set-first W))
      (define-values (block succs) (gobble-terms new-term))

      (block-set/add-succ parent-block block)

      (define unseen-tagged-succs
        (set-subtract (make-succ-pairs block succs) Seen))

      (loop (set-union (set-rest W) unseen-tagged-succs)
            (set-union Seen unseen-tagged-succs))]))

  (define-values (initial-block succs)
    (gobble-terms initial-term))

  (define tagged-succs (make-succ-pairs initial-block succs))

  (loop tagged-succs
        (set-union (make-succ-pairs #f succs)
                   tagged-succs))

  initial-block)

(define (make-succ-pairs parent succs)
  (for/set ([succ succs]) (list parent succ)))

(define push-term? (compose push? pda-term-insn))
(define go-term? (compose go? pda-term-insn))
(define token-case-term? (compose state-case? pda-term-insn))
(define state-case-term? (compose token-case? pda-term-insn))
(define if-eos-term? (compose if-eos? pda-term-insn))

(define-syntax-rule (f-and f ...) (lambda xs (and (apply f xs) ...)))
(define-syntax-rule (f-or f ...) (lambda xs (or (apply f xs) ...)))

(define branching-term?
  (f-and token-case-term?
         state-case-term?
         if-eos-term?))

(define starts-new-block?
  (f-or push-term?
        pop-assign-term?
        go-term?
        branching-term?
        ))

(define (gobble-terms term)
  (define (gobble-first-term/acc term)
    (cond [(branching-term? term)
           (new-branch-block (list term) (pda-term-succs term))]
          [(starts-new-block? term)
           (new-basic-block (list term) (set-first (pda-term-succs term)))]
          [else (gobble-terms/acc (set-first (pda-term-succs term))
                                  (list term))]))
  (define (gobble-terms/acc term terms)
    (if (starts-new-block? term)
        (new-basic-block (reverse terms) term)
        (gobble-terms/acc (set-first (pda-term-succs term))
                          (cons term terms))))

  (gobble-first-term/acc term))

