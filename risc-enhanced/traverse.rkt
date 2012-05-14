#lang racket
(require "data.rkt")
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
                          #:insn* [touch-insn* id]
                          #:term [touch-term id]
                          #:term* [touch-term* id]
                          #:seq-cons [seq-cons cons]
                          #:seq-end  [seq-end  empty]
                          #:pdarisc [touch-pdarisc id])
  (define syntax syntax*)
  (define reg reg*)
  (define reguse (compose reg reguse*))
  (define regdef (compose reg regdef*))
  (define lbl lbl*)
  (define lbluse (compose lbl lbluse*))
  (define lbldef (compose lbl lbldef*))
  (define (rhs r)
    (rhs* (match r
            ((state id) (state (syntax id)))
            ((register _ _ _ _) (reguse r))
            (other other))))
  (define token (compose syntax token*))


  (define (map/term-seq* seq)
    (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
          [(empty? (rest seq)) (seq-cons (map/term* (first seq)) seq-end)]
          [else (seq-cons (map/term (first seq)) (map/term-seq* (rest seq)))]))
  (define (map/term-seq seq)
    (foldr (lambda (x y)
             (seq-cons (map/term x) y))
           seq-end
           seq))
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
       ((join-point label args)
        (join-point (lbluse label) (map regdef args)))
       ((block seq) (block (map/term-seq seq))))))
  (define (map/term t)
    (match t
      ((pda-term a b c d i)
       (touch-term (pda-term a b c d (map/insn i))))))
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
               (map map/term-seq* bodies)
               (map/term-seq* body)))
       ((block* seq)
        (block* (map/term-seq* seq)))
       ((accept vals)
        (accept (map reguse vals)))
       ((reject) i)
       ((if-eos cnsq altr)
        (if-eos (map/term* cnsq)
                (map/term* altr)))
       ((state-case st lookaheads cnsqs)
        (state-case (reguse st)
                    (map rhs lookaheads)
                    (map map/term-seq* cnsqs)))
       ((token-case lookaheads cnsqs)
        (token-case (map (maybe-f token) lookaheads)
                    (map map/term-seq* cnsqs)))
       ((go target args)
        (go (lbluse target)
            (map rhs args))))))
  (define (map/term* t)
    (match t
      ((pda-term a b c d i)
       (touch-term (pda-term a b c d (map/insn* i))))))
  (values
   (lambda (pr)
     (match pr
       ((pdarisc seq) (touch-pdarisc (pdarisc (map/term-seq* seq))))))
   map/term
   map/term*))
