#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt"))
(provide set-preds!)

(define (preds-set-union! t elements)
  (enh:set-pda-term-preds! t (set-union (enh:pda-term-preds t) elements)))
(define (succs-set-union! t elements)
  (enh:set-pda-term-succs! t (set-union (enh:pda-term-succs t) elements)))
(define (succs-set-add! t element)
  (enh:set-pda-term-succs! t (set-add (enh:pda-term-succs t) element)))

(define (set-preds! pr)
  (match pr
    ((pdarisc seq)
     (set-preds/term-seq*! seq (seteq)))))

(define (set-preds/term-seq*! seq preds)
  (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
        [(empty? (rest seq)) (set-preds/term*! (first seq) preds)]
        [else (set-preds/term! (first seq)
                               preds
                               (first (rest seq)))
              (set-preds/term-seq*! (rest seq) (seteq (first seq)))]))

(define (set-preds/term! t preds succ)
  (preds-set-union! t preds)
  (succs-set-add! t succ))

(define (set-preds/term*! t preds)
  (preds-set-union! t preds)
  (succs-set-union! t (get-succs/term* t))
  (match (enh:pda-term-insn t)
    ((label ids _ _ param-lists bodies body)
     (for ((id ids)
           (body bodies))
       (set-preds/term-seq*! body
                             (enh:label-name-uses id)))
     (set-preds/term-seq*! body (seteq t)))
    ((block* insns)
     (set-preds/term-seq*! insns (seteq t)))
    ((if-eos cnsq altr)
     (set-preds/term*! cnsq (seteq t))
     (set-preds/term*! altr (seteq t)))
    ((state-case st lookaheads cnsqs)
     (for ((cnsq cnsqs))
       (set-preds/term-seq*! cnsq (seteq t))))
    ((token-case lookaheads cnsqs)
     (for ((cnsq cnsqs))
       (set-preds/term-seq*! cnsq (seteq t))))
    ((or (accept _)
         (go _ _)
         (reject))
     (void))
    (_ (error 'set-preds/term*! "did you add a new insn*? ~a" (enh:pda-term-insn t)))))

(define (get-succs/insn* i)
  (match i
    ((label ids stack-types token-types
            param-lists bodies body)
     (seteq (first body)))
    ((block* insns)
     (seteq (first insns)))
    ((accept vals)
     (seteq))
    ((reject)
     (seteq))
    ((if-eos cnsq altr)
     (seteq cnsq altr))
    ((state-case st lookaheads cnsqs)
     (for/seteq ((cnsq cnsqs))
       (first cnsq)))
    ((token-case lookaheads cnsqs)
     (for/seteq ((cnsq cnsqs))
       (first cnsq)))
    ((go target args)
     (seteq (enh:label-name-binding target)))))
(define get-succs/term* (enh:raise-input-to-term get-succs/insn*))

