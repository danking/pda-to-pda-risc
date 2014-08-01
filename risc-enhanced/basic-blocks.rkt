#lang racket

(provide (all-defined-out)
         (all-from-out "basic-block-data.rkt"))
(require "data.rkt"
         "basic-block-data.rkt"
         "../uid.rkt"
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
  (reset-uid)
  ;; bbmap : Term -> Block
  (define bbmap (make-hash))

  (define (loop W)
    (cond
     [(set-empty? W) (void)]
     [else
      (match-define (list parent-block new-term) (set-first W))
      (cond
       [(hash-has-key? bbmap new-term)
        (block-set/add-succ! parent-block (hash-ref bbmap new-term))
        (loop (set-rest W))]
       [else
        (define-values (block succs) (gobble-terms new-term))

        (hash-set-many! bbmap (in-list (block-nodes block)) block)

        (block-set/add-succ! parent-block block)

        (define-values (seen-succs unseen-succs)
          (set-partition succs (curry hash-has-key? bbmap)))

        (for ([seen-succ seen-succs])
          (block-set/add-succ! block (hash-ref bbmap seen-succ)))

        (loop (set-union (set-rest W) (make-succ-pairs block unseen-succs)))])]))

  (define-values (initial-block succs)
    (gobble-terms initial-term))

  (hash-set-many! bbmap (block-nodes initial-block) initial-block)

  (loop (make-succ-pairs initial-block succs))

  (values initial-block bbmap))

(define (make-succ-pairs parent succs)
  (for/seteq ([succ succs]) (list parent succ)))

(define push-term? (compose push? pda-term-insn))
(define go-term? (compose go? pda-term-insn))
(define token-case-term? (compose state-case? pda-term-insn))
(define state-case-term? (compose token-case? pda-term-insn))
(define if-eos-term? (compose if-eos? pda-term-insn))

(define-syntax-rule (f-or f ...) (lambda xs (or (apply f xs) ...)))

(define branching-term?
  (f-or token-case-term?
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
           (values (new-branch-block (next-uid)
                                     term
                                     (set))
                   (pda-term-succs term))]
          [(or (starts-new-block? term)
               (set-empty? (pda-term-succs term)))
           (values (new-basic-block (next-uid)
                                    (list term)
                                    #f)
                   (pda-term-succs term))]
          [else (gobble-terms/acc (set-first (pda-term-succs term))
                                  (list term))]))
  (define (gobble-terms/acc term terms)
    (cond [(starts-new-block? term)
           (values (new-basic-block (next-uid) (reverse terms) #f)
                   (seteq term))]
          [(set-empty? (pda-term-succs term))
           (values (new-basic-block (next-uid) (reverse terms) #f)
                   (seteq))]
          [else (gobble-terms/acc (set-first (pda-term-succs term))
                                  (cons term terms))]))

  (gobble-first-term/acc term))

(define (hash-set-many! h keys value)
  (for ([key keys])
    (hash-set! h key value)))

(define (set-partition s p)
  (for/fold
      ([s1 (set)]
       [s2 (set)])
      ([e (in-set s)])
    (if (p e)
        (values (set-add s1 e) s2)
        (values s1 (set-add s2 e)))))
