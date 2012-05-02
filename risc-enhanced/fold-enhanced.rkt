#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt"))
(provide fold/enhanced-pdarisc
         map-insn-combine)

(define-for-syntax (term-seq* recur recur*)
  (lambda (seq)
    (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
          [(empty? (rest seq)) (seq-combine (recur* (first seq)) seq-base)]
          [else (seq-combine (recur (first seq)) (recur* (rest seq)))])))

(define-syntax match-insn/recur
  (syntax-rules ()
    ((_ recur regdef reguse rhs i rules ...)
     (match i
       rules ...
       ((assign id val)
        (assign (regdef id)
                (rhs val)))
       ((push val)
        (assign (rhs val)))
       ((sem-act name params retvars action)
        (sem-act name
                 (map reguse params)
                 (map (lambda (rn)
                        (if rn
                            (regdef rn)
                            rn))
                      retvars)
                 action))
       ((drop-token) i)
       ((get-token) i)
       ((stack-ensure hdrm) i)
       ((block insns) (block (map recur insns)))
       (_ (error 'match-insn/recur "did you add a new insn?"))))))

(define-syntax match-insn*/recur
  (syntax-rules ()
    ((_ recur recur* labeldef labeluse regdef reguse rhs i rules ...)
     (let ((term-seq* (term-seq* recur recur*)))
       (match i
         rules ...
         ((label ids stack-types token-types
                 param-lists bodies body)
          (label (map labeldef ids)
                 stack-types
                 token-types
                 (map (lambda (param-list)
                        (map regdef param-list))
                      param-lists)
                 (map term-seq* bodies)
                 (term-seq* body)))
         ((block* insns)
          (block* (term-seq* insns)))
         ((accept vals)
          (accept (map reguse vals)))
         ((reject)
          i)
         ((if-eos cnsq altr)
          (if-eos (recur* cnsq)
                  (recur* altr)))
         ((state-case st lookaheads cnsqs)
          (state-case (reguse st)
                      lookaheads
                      (map term-seq* cnsqs)))
         ((token-case lookaheads cnsqs)
          (token-case lookaheads
                      (map term-seq* cnsqs)))
         ((go target args)
          (go (labeluse target)
              (map rhs args))))))))
