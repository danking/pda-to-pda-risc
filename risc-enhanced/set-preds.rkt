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
    ((pdarisc _ seq)
     (set-preds/term-seq*! seq (seteq)))))

(define (set-preds/term-seq*! seq preds)
  (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
        [(empty? (rest seq)) (set-preds/term*! (first seq) preds)]
        [else (let ((next-pred (set-preds/term! (first seq)
                                                preds
                                                (first (rest seq)))))
                (set-preds/term-seq*! (rest seq) (seteq next-pred)))]))

(define (set-preds/term-seq! seq preds succ)
  (cond [(empty? (rest seq))
         (set-preds/term! (first seq) preds succ)
         (first seq)]
        [else
         (set-preds/term! (first seq) preds (first (rest seq)))
         (set-preds/term-seq! (rest seq) (seteq (first seq)) succ)]))

(define (set-preds/term! t preds succ)
  (preds-set-union! t preds)
  (match (enh:pda-term-insn t)
    ((block _ (cons insn insns))
     (succs-set-add! t insn)
     (set-preds/term-seq! (cons insn insns) (seteq t) succ))
    (_ (succs-set-add! t succ)
       t)))

(define (set-preds/term*! t preds)
  (preds-set-union! t preds)
  (succs-set-union! t (get-succs/term* t))
  (match (enh:pda-term-insn t)
    ((label _ ids _ _ param-lists bodies body)
     (for ((id ids)
           (body bodies))
       (set-preds/term-seq*! body
                             (enh:label-name-uses id)))
     (set-preds/term-seq*! body (seteq t)))
    ((block* _ insns)
     (set-preds/term-seq*! insns (seteq t)))
    ((if-eos _ cnsq altr)
     (set-preds/term*! cnsq (seteq t))
     (set-preds/term*! altr (seteq t)))
    ((state-case _ st lookaheads cnsqs)
     (for ((cnsq cnsqs))
       (set-preds/term-seq*! cnsq (seteq t))))
    ((token-case _ lookaheads cnsqs)
     (for ((cnsq cnsqs))
       (set-preds/term-seq*! cnsq (seteq t))))
    ((or (accept _ _)
         (go _ _ _)
         (reject _))
     (void))
    (_ (error 'set-preds/term*! "did you add a new insn*? ~a" (enh:pda-term-insn t)))))

(define (get-succs/insn* i)
  (match i
    ((label _ ids stack-types token-types
            param-lists bodies body)
     (seteq (first body)))
    ((block* _ insns)
     (seteq (first insns)))
    ((accept _ vals)
     (seteq))
    ((reject _)
     (seteq))
    ((if-eos _ cnsq altr)
     (seteq cnsq altr))
    ((state-case _ st lookaheads cnsqs)
     (for/seteq ((cnsq cnsqs))
       (first cnsq)))
    ((token-case _ lookaheads cnsqs)
     (for/seteq ((cnsq cnsqs))
       (first cnsq)))
    ((go _ target args)
     (seteq (enh:label-name-binding target)))))
(define get-succs/term* (enh:raise-input-to-term get-succs/insn*))

