#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt"))
(provide fold/enhanced-pdarisc
         map-insn-combine)

(define (map-insn-combine i . args)
  (apply (struct-type-make-constructor (let-values (((a _) (struct-info i))) a))
         args))

(define (fold/enhanced-pdarisc pr
                               enhanced-combine
                               seq-combine
                               seq-base
                               insn-combine
                               register
                               labelname
                               purerhs
                               varrhs)
  (define (fold/term-seq* seq)
    (cond [(empty? seq) (error 'fold/term-seq* "cannot have an empty term-seq*")]
          [(empty? (rest seq)) (seq-combine (fold/term* (first seq)) seq-base)]
          [else (seq-combine (fold/term (first seq)) (fold/term-seq* (rest seq)))]))
  (define (fold/term-seq seq)
    (foldr (lambda (i seq)
             (seq-combine (fold/term i) seq))
           seq-base
           seq))
  (define (fold/term t)
    (match t
      ((enh:pda-term preds succs avail-regs live-regs i)
       (enhanced-combine preds succs avail-regs live-regs (fold/insn i)))))
  (define (fold/term* t)
    (match t
      ((enh:pda-term preds succs avail-regs live-regs i)
       (enhanced-combine preds succs avail-regs live-regs (fold/insn* i)))))
  (define (fold/insn i)
    (match i
      ((assign id val)
       (insn-combine i
                     (register id)
                     (varrhs val)))
      ((push val)
       (insn-combine i (purerhs val)))
      ((sem-act name params retvars action)
       (insn-combine i
                name
                (map register params)
                (map (lambda (rn)
                       (if rn
                           (register rn)
                           rn))
                     retvars)
                action))
      ((drop-token) (insn-combine i))
      ((get-token) (insn-combine i))
      ((stack-ensure hdrm) (insn-combine i hdrm))
      ((block insns) (insn-combine i (fold/term-seq insns)))))
  (define (fold/insn* i)
    (match i
      ((label ids stack-types token-types
              param-lists bodies body)
       (insn-combine i
                (map labelname ids)
                stack-types
                token-types
                (map (lambda (param-list)
                       (map register param-list))
                     param-lists)
                (map fold/term-seq* bodies)
                (fold/term-seq* body)))
      ((block* insns)
       (insn-combine i
                (fold/term-seq* insns)))
      ((accept vals)
       (insn-combine i (map register vals)))
      ((reject)
       (insn-combine i))
      ((if-eos cnsq altr)
       (insn-combine i
                (fold/term* cnsq)
                (fold/term* altr)))
      ((state-case st lookaheads cnsqs)
       (insn-combine i
                (register st)
                lookaheads
                (map fold/term-seq* cnsqs)))
      ((token-case lookaheads cnsqs)
       (insn-combine i
                lookaheads
                (map fold/term-seq* cnsqs)))
      ((go target args)
       (insn-combine i
                (labelname target)
                (map purerhs args)))))

  (match pr
    ((pdarisc insns) (pdarisc (fold/term-seq* insns)))))