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
                    label-name-id)
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
                             label-name-id)))
(provide (all-defined-out)
         ;; Re-exports
         (all-from-out "../pdarisc-data.rkt"))

;; a PDA-RISC-ENH is a:
;;   (make-pdarisc Term*-Seq)

;; a Term-Seq is a [ListOf Term]
;; a Term*-Seq is a (append Term-Seq (list Term*))

;; a Term is a (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       [SetEq Register]
;;                       [SetEq Register]
;;                       Insn)

;; a Term* is the same as a Term except it contains an Insn instead of an Insn*
;; concretely, (pda-term [SetEq Term]
;;                       [SetEq Term]
;;                       [SetEq Register]
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
  (if (insn*? (pda-term-insn t))
      (write (unparse-term* t) port)
      (write (unparse-term t) port)))

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
                                         (lambda (x r)
                                           (match x
                                             ((pda-term _ _ _ _ i)
                                              (r i))))
                                         (lambda (x r)
                                           (match x
                                             ((pda-term _ _ _ _ i)
                                              (- (r i))))))
        #:property prop:custom-write write-pda-term
        )

(define (pda-risc-enh-initial-term pre)
  (match pre
    ((pdarisc _ seq) (first seq))))

(define (uninitialized-register name)
  (register name #f #f (seteq)))
(define (register-equal? x y)
  (eq? (register-uid x) (register-uid y)))
(define (register-hash-code x)
  (register-uid x))
(define (register-secondary-hash-code x)
  (- (register-uid x)))
(define (write-register r port mode)
  (write (list 'register (register-uid r) (register-lexical-name r)) port))
(struct register (lexical-name
                  uid
                  [binding #:mutable]
                  [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (register-equal? x y))
                                         (lambda (x _) (register-hash-code x))
                                         (lambda (x _) (register-secondary-hash-code x)))
        #:property prop:custom-write write-register)
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
(define (write-label-name l port mode)
  (write (list 'label-name (label-name-uid l) (label-name-lexical-name l)) port))
;; (label-name String Number Join-Point [SetOf GoInsn])
(struct label-name (lexical-name
                    uid
                    [binding #:mutable]
                    [uses #:mutable])
        #:transparent
        #:property prop:equal+hash (list (lambda (x y _) (label-name-equal? x y))
                                         (lambda (x _) (label-name-hash-code x))
                                         (lambda (x _) (label-name-secondary-hash-code x)))
        #:property prop:custom-write write-label-name)
(define (label-name-add-use! r u)
  (set-label-name-uses! r (set-add (label-name-uses r) u)))
(define (get-label-params lblname)
  (join-point-params (label-name-binding lblname)))

;; a join-point is an insn
(struct join-point (uid label params) #:transparent)

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
       ((assign uid id val)
        (assign uid
                (regdef id)
                (rhs val)))
       ((push uid val)
        (push uid (rhs val)))
       ((sem-act uid name params retvars action)
        (sem-act uid
                 name
                 (map reguse params)
                 (map (lambda (rn)
                        (if rn
                            (regdef rn)
                            rn))
                      retvars)
                 action))
       ((drop-token _) i)
       ((get-token _) i)
       ((stack-ensure uid hdrm) (stack-ensure uid hdrm))
       ((join-point uid label args)
        (join-point uid (lbluse label) (map regdef args)))
       ((block uid seq) (block uid (map/term-seq seq))))))
  (define (map/term t)
    (match t
      ((pda-term a b c d i)
       (touch-term (pda-term a b c d (map/insn i))))))
  (define (map/insn* i)
    (touch-insn*
     (match i
       ((label uid ids stack-types token-types
               param-lists bodies body)
        (label uid
               (map lbldef ids)
               stack-types
               token-types
               (map (lambda (param-list)
                      (map regdef param-list))
                    param-lists)
               (map map/term-seq* bodies)
               (map/term-seq* body)))
       ((block* uid seq)
        (block* uid (map/term-seq* seq)))
       ((accept uid vals)
        (accept uid (map reguse vals)))
       ((reject _) i)
       ((if-eos uid cnsq altr)
        (if-eos uid
                (map/term* cnsq)
                (map/term* altr)))
       ((state-case uid st lookaheads cnsqs)
        (state-case uid
                    (reguse st)
                    (map rhs lookaheads)
                    (map map/term-seq* cnsqs)))
       ((token-case uid lookaheads cnsqs)
        (token-case uid
                    (map (maybe-f token) lookaheads)
                    (map map/term-seq* cnsqs)))
       ((go uid target args)
        (go uid
            (lbluse target)
            (map rhs args))))))
  (define (map/term* t)
    (match t
      ((pda-term a b c d i)
       (touch-term (pda-term a b c d (map/insn* i))))))
  (values
   (lambda (pr)
     (match pr
       ((pdarisc uid seq) (touch-pdarisc (pdarisc uid (map/term-seq* seq))))))
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
    ((assign uid id val)
     `(:= ,uid ,id ,val))
    ((push uid val)
     `(push ,uid ,val))
    ((sem-act uid name params retvars action)
     `(semantic-action ,uid
                       ,name
                       ,params
                       ,retvars
                       ,action))
    ((drop-token uid)
     `(drop-token ,uid))
    ((get-token uid)
     `(get-token ,uid))
    ((stack-ensure uid hdrm)
     `(stack-ensure ,uid ,hdrm))
    ((join-point uid label args)
     `(join-point ,uid ,label . ,args))
    ((block uid insns)
     `(block ,uid . ,insns))))

(define (unparse-insn* i)
  (match i
    ((label uid ids stack-types token-types param-lists rhses body)
     `(label ,uid
             (...)
             . ,body))
    ((block* uid insns)
     `(block ,uid . ,insns))
    ((accept uid vars)
     `(accept ,uid . ,vars))
    ((reject uid)
     `(reject ,uid))
    ((if-eos uid cnsq altr)
     `(if-eos ,uid ,cnsq ,altr))
    ((state-case uid var looks cnsqs)
     `(state-case ,uid ,var
                  . ,(map (lambda (look cnsq)
                            (cons look cnsq))
                          looks
                          cnsqs)))
    ((token-case uid looks cnsqs)
     `(token-case ,uid . ,(map (lambda (l c)
                            (cons (if l l #f) c))
                          looks
                          cnsqs)))
    ((go uid target args)
     `(go ,uid ,target . ,args))))

(define (strip-term t)
  (match t
    ((pda-term _ _ _ _ i)
     `(pda-term ,i))))

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

(define-values
  (unparse unparse-term unparse-term*)
  (traverse-pdarisc #:pdarisc (match-lambda ((pdarisc uid seq) seq))
                    #:term strip-term
                    #:term* strip-term
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
