#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         (for-syntax syntax/parse))
(provide match-insn*/recur match-insn/recur)

(define-syntax match-insn/recur
  (syntax-rules ()
    ((_ recur regdef reguse rhs i rules ...)
     (match i
       rules ...
       ((assign id val)
        (assign (regdef id)
                (rhs val)))
       ((push val)
        (push (rhs val)))
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
       ((enh:join-point params)
        (enh:join-point (map regdef params)))
       (_ (error 'match-insn/recur "did you add a new insn? ~a" i))))))

(define-for-syntax (identity x) x)
(define (iterate-term-seq* term term*)
  (lambda (seq)
    (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
          [(empty? (rest seq)) (list (term* (first seq)))]
          [else (cons (term (first seq)) ((iterate-term-seq* term term*) (rest seq)))])))
(define (raise-to-term insn-handler)
  (lambda (term)
    (match term
      ((enh:pda-term a b c d i)
       (enh:pda-term a b c d (insn-handler i))))))

(define-syntax (match-insn*/recur stx)
  (syntax-parse stx
    [(m (~or (~optional (~seq #:insn insn:expr) #:defaults ([insn #'identity]))
             (~optional (~seq #:insn* insn*:expr) #:defaults ([insn* #'identity]))
             (~optional (~seq #:term term:expr) #:defaults ([term #'#f]))
             (~optional (~seq #:term* term*:expr) #:defaults ([term* #'#f]))
             (~optional (~seq #:labeldef labeldef:expr) #:defaults ([labeldef #'identity]))
             (~optional (~seq #:labeluse labeluse:expr) #:defaults ([labeluse #'identity]))
             (~optional (~seq #:regdef regdef:expr) #:defaults ([regdef #'identity]))
             (~optional (~seq #:reguse reguse:expr) #:defaults ([reguse #'identity]))
             (~optional (~seq #:rhs rhs:expr) #:defaults ([rhs #'identity]))
             (~optional (~seq #:term-seq* term-seq*:expr) #:defaults ([term-seq* #'#f])))
        ...
        i:expr
        rules ...)
     #'(let* ((term-handler (if term term (raise-to-term insn)))
              (term*-handler (if term* term* (raise-to-term insn*)))
              (term-seq*-handler (if term-seq* term-seq*
                                     (iterate-term-seq* term-handler term*-handler))))
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
                   (map term-seq*-handler bodies)
                   (term-seq*-handler body)))
           ((block* insns)
            (block* (term-seq*-handler insns)))
           ((accept vals)
            (accept (map reguse vals)))
           ((reject)
            i)
           ((if-eos cnsq altr)
            (if-eos (term*-handler cnsq)
                    (term*-handler altr)))
           ((state-case st lookaheads cnsqs)
            (state-case (reguse st)
                        lookaheads
                        (map term-seq*-handler cnsqs)))
           ((token-case lookaheads cnsqs)
            (token-case lookaheads
                        (map term-seq*-handler cnsqs)))
           ((go target args)
            (go (labeluse target)
                (map rhs args)))
           (_ (error 'match-insn*/recur "did you add a new insn*? ~a" i))))]))
