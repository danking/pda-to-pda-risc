#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         "../../racket-utils/environment.rkt")

(provide (all-defined-out))

(define (set-uids pr)
  (match pr
    ((pdarisc seq) (pdarisc (su/term-seq* seq empty-env empty-env)))))

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
      ((assign id val) (assign (su/reg-def id) (su/rhs val)))
      ((push val)      (push (su/rhs val)))
      ((sem-act name params retvars action)
       (sem-act name
                (map (wrap-maybe su/reg-use) params)
                (map su/reg-def retvars)
                action))
      ((drop-token) i)
      ((get-token)  i)
      ((stack-ensure hdrm) i)
      ((block seq) (block (su/term-seq seq reg-e lbl-e)))
      ((enh:join-point lbl params)
       (enh:join-point (su/lbl-join-point lbl t lbl-e)
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
      ((label ids stack-types token-types
              param-lists bodies body)
       (let* ((updated-ids (map (lambda (l) (su/lbl-alloc l t lbl-e)) ids))
              (new-lbl-e (extend-env/labels lbl-e updated-ids)))
         (set-label-ids! i updated-ids)
         (set-label-bodies! i (for/list ([body bodies])
                                (su/term-seq* body reg-e new-lbl-e)))
         (set-label-body! i (su/term-seq* body reg-e new-lbl-e))
         i))
      ((block* insns)
       (block* (su/term-seq*/rec insns)))
      ((accept vals)
       (accept (map su/reg-use vals)))
      ((reject)
       i)
      ((if-eos cnsq altr)
       (if-eos (su/term*/rec cnsq)
               (su/term*/rec altr)))
      ((state-case st lookaheads cnsqs)
       (state-case (su/reg-use st)
                   lookaheads
                   (map su/term-seq*/rec cnsqs)))
      ((token-case lookaheads cnsqs)
       (token-case lookaheads
                   (map su/term-seq*/rec cnsqs)))
      ((go target args)
       (go (su/lbl-use target t lbl-e) (map su/rhs args)))
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
    ((assign reg _)
     (extend-env/regs reg-e (list reg)))
    ((sem-act _ _ regs _)
     (extend-env/regs reg-e regs))
    ((enh:join-point _ regs)
     (extend-env/regs reg-e regs))
    ((block seq)
     (for/fold ((reg-e reg-e))
               ((t seq))
       (add-new-reg-bindings-from-term t reg-e)))
    (_ reg-e)))
(define add-new-reg-bindings-from-term (enh:raise-input-to-term add-new-reg-bindings-from-insn))

(define-values (next-id reset-id)
  (let ((id -1))
    (values (lambda ()
              (set! id (add1 id))
              id)
            (lambda ()
              (set! id -1)))))

(define (su/rhs rhs t reg-e)
  (if (enh:register? rhs)
      (su/reg-use rhs t reg-e)
      rhs))

(define (su/reg-def r t reg-e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (next-id) t u))))

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
     (enh:label-name name (next-id) b u))))

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
