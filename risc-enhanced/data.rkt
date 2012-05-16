#lang racket
(require (except-in "../pdarisc-data.rkt"
                    register
                    struct:register
                    make-register
                    register?

                    named-reg
                    struct:named-reg
                    make-named-reg
                    named-reg?
                    named-reg-id

                    nameless-reg
                    struct:nameless-reg
                    make-nameless-reg
                    nameless-reg?

                    label-name
                    struct:label-name
                    make-label-name
                    label-name?
                    label-name-id

                    label-polynym
                    struct:label-polynym
                    make-label-polynym
                    label-polynym?
                    label-polynym-extra-id

                    label-name->symbol)
         (prefix-in old:
                    (only-in "../pdarisc-data.rkt"
                             register
                             struct:register
                             make-register
                             register?

                             named-reg
                             struct:named-reg
                             make-named-reg
                             named-reg?
                             named-reg-id

                             nameless-reg
                             struct:nameless-reg
                             make-nameless-reg
                             nameless-reg?

                             label-name
                             struct:label-name
                             make-label-name
                             label-name?
                             label-name-id

                             label-polynym
                             struct:label-polynym
                             make-label-polynym
                             label-polynym?
                             label-polynym-extra-id

                             label-name->symbol)))
(provide (all-defined-out)
         ;; Re-exports
         (all-from-out "../pdarisc-data.rkt"))

;; a PDA-RISC-ENH term is a:
;;   (make-pdarisc Term*-Seq)

;; a Term-Seq is a [ListOf Term]
;; a Term*-Seq is a (append Term-Seq (list Term*))

;; a Term is a (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       Register
;;                       [SetEq Register]
;;                       Insn)

;; a Term* is the same as a Term except it contains an Insn instead of an Insn*
;; concretely, (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       Register
;;                       [SetEq Register]
;;                       Insn*)

;; Insn and Insn* are defined in ../pdarisc-data.rkt
;; Except:
;;   - instances of Register are replaced with the register struct defined
;;     in this file, and all
;;   - instances of LabelName are replaced with the label-name struct in this
;;     file
;;   - every labeled insn-seq* in a label form has a join-point appended to it
;;     which records the label and argument list associated with that insn-seq*

(define (uninitialized-pda-term insn)
  (pda-term (seteq) (seteq)
            #f #f
            insn))

(define (write-pda-term t port mode)
  (cond [(number? mode) (print   (unparse t) port mode)]
        [(false? mode)  (display (unparse t) port)]
        [else           (write   (unparse t) port)]))

(struct pda-term (preds
                  succs
                  avail-regs
                  live-regs
                  insn)
        #:mutable
        #:transparent
        #:property prop:equal+hash (list (lambda (x y recur-equal?)
                                           (match (list x y)
                                             ((list (pda-term _ _ _ _ i1)
                                                    (pda-term _ _ _ _ i2))
                                              (recur-equal? i1 i2))))
                                         (lambda (x _)
                                           (eq-hash-code x))
                                         (lambda (x _)
                                           (- (eq-hash-code x))))
        #:property prop:custom-write write-pda-term)

(define (pda-risc-enh-initial-term pre)
  (match pre
    ((pdarisc seq) (first seq))))

(define (uninitialized-register name)
  (register name #f #f (seteq)))
(define (register-equal? x y)
  (eq? (register-uid x) (register-uid y)))
(define (register-hash-code x)
  (register-uid x))
(define (register-secondary-hash-code x)
  (- (register-uid x)))
(struct register (lexical-name
                  uid
                  [binding #:mutable]
                  [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (register-equal? x y))
                                         (lambda (x _) (register-hash-code x))
                                         (lambda (x _) (register-secondary-hash-code x))))
(define (register-add-use! r u)
  (set-register-uses! r (set-add (register-uses r) u)))

(define (uninitialized-label-name name)
  (label-name name #f #f (seteq)))
(define (label-name-equal? x y)
  (eq? (label-name-uid x) (label-name-uid y)))
(define (label-name-hash-code x)
  (label-name-uid x))
(define (label-name-secondary-hash-code x)
  (- (label-name-uid x)))
(struct label-name (lexical-name
                    uid
                    [binding #:mutable]
                    [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (label-name-equal? x y))
                                         (lambda (x _) (label-name-hash-code x))
                                         (lambda (x _) (label-name-secondary-hash-code x))))
(define (label-name-add-use! r u)
  (set-label-name-uses! r (set-add (label-name-uses r) u)))

;; a join-point is an insn
(struct join-point (label params) #:transparent)

(define (raise-to-term f)
  (lambda (t . rest)
    (match t
      ((pda-term a b c d i)
       (pda-term a b c d (apply f (cons i rest)))))))

(define (raise-input-to-term f)
  (lambda (t . rest)
    (match t
      ((pda-term a b c d i)
       (apply f (cons i rest))))))


;; General Fold
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

;; printer

(define (unparse-rhs r)
  (match r
    ((pop) `(pop))
    ((state id)
     `(state ,id))
    ((nterm id)
     `(nterm ,id))
    ((curr-token #f)
     '(current-token))
    ((curr-token n)
     `(current-token ,n))
    (_ r)))

(define (unparse-insn i)
  (match i
    ((assign id val)
     `(:= ,id ,val))
    ((push val)
     `(push ,val))
    ((sem-act name params retvars action)
     `(semantic-action ,name
                       ,params
                       ,retvars
                       ,action))
    ((drop-token)
     'drop-token)
    ((get-token)
     'get-token)
    ((stack-ensure hdrm)
     `(stack-ensure ,hdrm))
    ((join-point label args)
     `(join-point ,label . ,args))
    ((block insns)
     `(block . ,insns))))

(define (unparse-insn* i)
  (match i
   ((label ids stack-types token-types param-lists rhses body)
    `(label ,(unparse-label-clauses ids stack-types token-types param-lists rhses)
            . ,body))
   ((block* insns)
    `(block . ,insns))
   ((accept vars)
    `(accept . ,vars))
   ((reject)
    `(reject))
   ((if-eos cnsq altr)
    `(if-eos ,cnsq ,altr))
   ((state-case var looks cnsqs)
    `(state-case ,var
                 . ,(map (lambda (look cnsq)
                           (cons look cnsq))
                         looks
                         cnsqs)))
   ((token-case looks cnsqs)
    `(token-case . ,(map (lambda (l c)
                           (cons (if l l #f) c))
                         looks
                         cnsqs)))
   ((go target args)
    `(go ,target . ,args))))

(define (unparse-label-clauses ids stack-types token-types param-lists rhses)
  (map (lambda (id stack-type token-type param rhs)
         `(,id : ,stack-type
               ,token-type
               ,param
               . ,rhs))
       ids
       stack-types
       token-types
       param-lists
       rhses))

(define (remove-struct:-prefix sym)
  (string->symbol (substring (symbol->string sym) 7)))

(define-values
  (unparse unparse-term unparse-term*)
  (traverse-pdarisc #:pdarisc (match-lambda ((pdarisc seq) seq))
                    #:term pda-term-insn
                    #:term* pda-term-insn
                    #:insn unparse-insn
                    #:insn* unparse-insn*
                    #:rhs unparse-rhs
                    #:syntax syntax-e
                    #:lbl (lambda (l)
                            `(label-name ,(label-name-uid l)
                                         ,(label-name-lexical-name l)))
                    #:reg (lambda (r)
                            `(register ,(register-uid r)
                                       ,(register-lexical-name r)))))
