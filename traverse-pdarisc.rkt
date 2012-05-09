#lang racket
(require "pdarisc-data.rkt")
(provide traverse-pdarisc)

(define (id x) x)
(define (maybe-f f) (lambda (x) (if x (f x) x)))

(define (traverse-pdarisc #:reguse [reguse* id]
                          #:regdef [regdef* id]
                          #:reg [reg* id]
                          #:lbluse [lbluse* id]
                          #:lbldef [lbldef* id]
                          #:lbl [lbl* id]
                          #:rhs [rhs* id]
                          #:token [token* id]
                          #:syntax [syntax* id]
                          #:insn [touch-insn id]
                          #:insn* [touch-insn* id])
  (define syntax syntax*)
  (define (reg r)
    (match (reg* r)
      ((named-reg id) (named-reg (syntax id)))
      (r r)))
  (define reguse (compose reg reguse*))
  (define regdef (compose reg regdef*))
  (define (lbl l)
    (match (lbl* l)
      ((label-polynym id x) (label-polynym (syntax id) x))
      ((label-name id) (label-name (syntax id)))))
  (define lbluse (compose lbl lbluse*))
  (define lbldef (compose lbl lbldef*))
  (define (rhs r)
    (match (rhs* r)
      ((state id) (state (syntax id)))
      ((and (register) reg) (reguse reg))
      (other other)))
  (define token (compose syntax token*))


  (define (map/insn-seq* seq)
    (cond [(empty? seq) (error 'insn-seq* "cannot have an empty insn-seq*")]
          [(empty? (rest seq)) (list (map/insn* (first seq)))]
          [else (cons (map/insn (first seq)) (map/insn-seq* (rest seq)))]))
  (define (map/insn-seq seq)
    (map map/insn seq))
  (define (map/insn i)
    (touch-insn
     (match i
       ((assign id val)
        (assign (regdef id)
                (rhs val)))
       ((push val)
        (push (rhs val)))
       ((sem-act name params retvars action)
        (sem-act (syntax name)
                 (map reguse params)
                 (map (lambda (rn)
                        (if rn
                            (regdef rn)
                            rn))
                      retvars)
                 action))
       ((drop-token) i)
       ((get-token) i)
       ((stack-ensure hdrm) (stack-ensure hdrm))
       ((block insns) (block (map/insn-seq insns))))))
  (define (map/insn* i)
    (touch-insn*
     (match i
       ((label ids stack-types token-types
               param-lists bodies body)
        (label (map lbldef ids)
               stack-types
               token-types
               (map (lambda (param-list)
                      (map regdef param-list))
                    param-lists)
               (map map/insn-seq* bodies)
               (map/insn-seq* body)))
       ((block* insns)
        (block* (map/insn-seq* insns)))
       ((accept vals)
        (accept (map reguse vals)))
       ((reject) i)
       ((if-eos cnsq altr)
        (if-eos (map/insn* cnsq)
                (map/insn* altr)))
       ((state-case st lookaheads cnsqs)
        (state-case (reguse st)
                    (map rhs lookaheads)
                    (map map/insn-seq* cnsqs)))
       ((token-case lookaheads cnsqs)
        (token-case (map (maybe-f token) lookaheads)
                    (map map/insn-seq* cnsqs)))
       ((go target args)
        (go (lbluse target)
            (map rhs args))))))
  (values
   (lambda (pr)
     (match pr
       ((pdarisc seq) (pdarisc (map/insn-seq* seq)))))
   map/insn
   map/insn*))