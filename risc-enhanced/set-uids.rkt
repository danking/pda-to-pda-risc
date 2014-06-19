#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         "../../racket-utils/environment.rkt"
         "../uid.rkt")

(provide (all-defined-out))

(define (set-uids pr)
  (match pr
    ((pdarisc uid _ _ seq)
     ;; evaluate the term-seq first so that {reg,lbl}-current-uid is correct
     (let ((seq (su/term-seq* seq empty-env empty-env)))
       (pdarisc uid (reg-current-uid) (lbl-current-uid) seq)))))

(define (su/term t reg-e lbl-e)
  (match t
    ((enh:pda-term a b c d i)
     (enh:set-pda-term-insn! t (su/insn i t reg-e lbl-e))
     t)))

(define (su/term* t reg-e lbl-e)
  (match t
    ((enh:pda-term a b c d i)
     (enh:set-pda-term-insn! t (su/insn* i t reg-e lbl-e))
     t)))

(define (su/insn i t reg-e lbl-e)
  (define (wrap f) (lambda (x) (f x t reg-e)))

  (let ((su/reg-def (wrap su/reg-def))
        (su/reg-use (wrap su/reg-use))
        (su/rhs     (wrap su/rhs)))
    (match i
      ((assign uid id val) (assign uid (su/reg-def id) (su/rhs val)))
      ((push uid val) (push uid (su/rhs val)))
      ((sem-act uid name params retvars action)
       (sem-act uid
                name
                (map (wrap-maybe su/reg-use) params)
                (map su/reg-def retvars)
                action))
      ((drop-token _) i)
      ((get-token _) i)
      ((stack-ensure _ hdrm) i)
      ((block uid seq) (block uid (su/term-seq seq reg-e lbl-e)))
      ((enh:join-point uid lbl params)
       (enh:join-point uid
                       (su/lbl-join-point lbl t lbl-e)
                       (map su/reg-def params)))
      (_ (error 'su/insn "did you add a new insn? ~a" i)))))

(define (su/insn* i t reg-e lbl-e)
  (define (wrap-no-lbl f) (lambda (x) (f x t reg-e)))
  (define (wrap f) (lambda (x) (f x reg-e lbl-e)))

  (let ((su/reg-def (wrap-no-lbl su/reg-def))
        (su/reg-use (wrap-no-lbl su/reg-use))
        (su/rhs     (wrap-no-lbl su/rhs))
        (su/term-seq*/rec (wrap su/term-seq*))
        (su/term*/rec     (wrap su/term*)))
    (match i
      ((label _ ids stack-types token-types
              param-lists bodies body)
       (let* ((updated-ids (map (lambda (l) (su/lbl-alloc l t lbl-e)) ids))
              (new-lbl-e (extend-env/labels lbl-e updated-ids)))
         (set-label-ids! i updated-ids)
         (set-label-bodies! i (for/list ([body bodies])
                                (su/term-seq* body reg-e new-lbl-e)))
         (set-label-body! i (su/term-seq* body reg-e new-lbl-e))
         i))
      ((block* uid insns)
       (block* uid (su/term-seq*/rec insns)))
      ((accept uid vals)
       (accept uid (map su/reg-use vals)))
      ((reject _)
       i)
      ((if-eos uid cnsq altr)
       (if-eos uid
               (su/term*/rec cnsq)
               (su/term*/rec altr)))
      ((state-case uid st lookaheads cnsqs)
       (state-case uid
                   (su/reg-use st)
                   lookaheads
                   (map su/term-seq*/rec cnsqs)))
      ((token-case uid lookaheads cnsqs)
       (token-case uid
                   lookaheads
                   (map su/term-seq*/rec cnsqs)))
      ((go uid target args)
       (go uid (su/lbl-use target t lbl-e) (map su/rhs args)))
      (_ (error 'su/insn* "did you add a new insn*? ~a" i)))))

(define (su/term-seq seq reg-e lbl-e)
  (if (empty? seq)
      empty
      (let ((updated-term (su/term (first seq) reg-e lbl-e)))
        (cons updated-term
              (su/term-seq (rest seq)
                           (add-new-reg-bindings-from-term updated-term reg-e)
                           lbl-e)))))

(define (su/term-seq* seq reg-e lbl-e)
  (cond [(empty? seq) (error 'configure-registers "term-seq* should always end in an insn*")]
        [(empty? (rest seq)) (list (su/term* (first seq) reg-e lbl-e))]
        [else
         (let ((updated-term (su/term (first seq) reg-e lbl-e)))
           (cons updated-term
                 (su/term-seq* (rest seq)
                               (add-new-reg-bindings-from-term updated-term reg-e)
                               lbl-e)))]))

(define (add-new-reg-bindings-from-insn i reg-e)
  (match i
    ((assign _ reg _)
     (extend-env/regs reg-e (list reg)))
    ((sem-act _ _ _ regs _)
     (extend-env/regs reg-e regs))
    ((enh:join-point _ _ regs)
     (extend-env/regs reg-e regs))
    ((block _ seq)
     (for/fold ((reg-e reg-e))
               ((t seq))
       (add-new-reg-bindings-from-term t reg-e)))
    (_ reg-e)))
(define add-new-reg-bindings-from-term (enh:raise-input-to-term add-new-reg-bindings-from-insn))

(define-values
  (reg-next-uid reg-current-uid reg-reset-uid reg-set-uid)
  (init))
(define-values
  (lbl-next-uid lbl-current-uid lbl-reset-uid lbl-set-uid)
  (init))

(define (su/rhs rhs t reg-e)
  (if (enh:register? rhs)
      (su/reg-use rhs t reg-e)
      rhs))

(define (su/reg-def r t reg-e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (reg-next-uid) t u))))

(define (su/reg-use r t reg-e)
  (match r
    ((enh:register name _ b u)
     (let ((reg (lookup-env reg-e name)))
       (enh:register-add-use! reg t)
       reg))))

(define (su/lbl-join-point lbl t lbl-e)
  (match lbl
    ((enh:label-name name _ _ _)
     (let ((label (lookup-env lbl-e name)))
       (enh:set-label-name-binding! label t)
       label))))

(define (su/lbl-alloc r t lbl-e)
  (match r
    ((enh:label-name name _ b u)
     (enh:label-name name (lbl-next-uid) b u))))

(define (su/lbl-use r t lbl-e)
  (match r
    ((enh:label-name name _ b u)
     (let ((label (lookup-env lbl-e name)))
       (enh:label-name-add-use! label t)
       label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities
(define (extend-env/regs e rs)
  (extend-env/list e
                   (map enh:register-lexical-name rs)
                   rs))
(define (extend-env/labels e ls)
  (extend-env/list e
                   (map enh:label-name-lexical-name ls)
                   ls))

(define (wrap-maybe f)
  (lambda (x) (if x (f x) x)))
