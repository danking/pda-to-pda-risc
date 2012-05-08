#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         "../../racket-utils/environment.rkt")

(provide (all-defined-out))

(define (set-uids/pdarisc pr)
  (match pr
    ((pdarisc seq) (pdarisc (cr/term-seq* seq empty-env empty-env)))))

(define (cr/term t reg-e lbl-e)
  (match t
    ((enh:pda-term a b c d i)
     (enh:set-pda-term-insn! t (cr/insn i t reg-e lbl-e))
     t)))

(define (cr/term* t reg-e lbl-e)
  (match t
    ((enh:pda-term a b c d i)
     (enh:set-pda-term-insn! t (cr/insn* i t reg-e lbl-e))
     t)))

(define (cr/insn i t reg-e lbl-e)
  (define (wrap f) (lambda (x) (f x t reg-e)))

  (let ((cr/reg-def (wrap cr/reg-def))
        (cr/reg-use (wrap cr/reg-use))
        (cr/rhs     (wrap cr/rhs)))
    (match i
      ((assign id val) (assign (cr/reg-def id) (cr/rhs val)))
      ((push val)      (push (cr/rhs val)))
      ((sem-act name params retvars action)
       (sem-act name
                (map (wrap-maybe cr/reg-use) params)
                (map cr/reg-def retvars)
                action))
      ((drop-token) i)
      ((get-token)  i)
      ((stack-ensure hdrm) i)
      ((block seq) (block (cr/term-seq seq reg-e lbl-e)))
      ((enh:join-point lbl params)
       (enh:join-point (cr/lbl-join-point lbl t lbl-e)
                       (map cr/reg-def params)))
      (_ (error 'cr/insn "did you add a new insn? ~a" i)))))

(define (cr/insn* i t reg-e lbl-e)
  (define (wrap-no-lbl f) (lambda (x) (f x t reg-e)))
  (define (wrap f) (lambda (x) (f x reg-e lbl-e)))

  (let ((cr/reg-def (wrap-no-lbl cr/reg-def))
        (cr/reg-use (wrap-no-lbl cr/reg-use))
        (cr/rhs     (wrap-no-lbl cr/rhs))
        (cr/term-seq*/rec (wrap cr/term-seq*))
        (cr/term*/rec     (wrap cr/term*)))
    (match i
      ((label ids stack-types token-types
              param-lists bodies body)
       (let* ((updated-ids (map (lambda (l) (cr/lbl-alloc l t lbl-e)) ids))
              (new-lbl-e (extend-env/labels lbl-e updated-ids)))
         (set-label-ids! i updated-ids)
         (set-label-bodies! i (for/list ([body bodies])
                                (cr/term-seq* body reg-e new-lbl-e)))
         (set-label-body! i (cr/term-seq* body reg-e new-lbl-e))
         i))
      ((block* insns)
       (block* (cr/term-seq*/rec insns)))
      ((accept vals)
       (accept (map cr/reg-use vals)))
      ((reject)
       i)
      ((if-eos cnsq altr)
       (if-eos (cr/term*/rec cnsq)
               (cr/term*/rec altr)))
      ((state-case st lookaheads cnsqs)
       (state-case (cr/reg-use st)
                   lookaheads
                   (map cr/term-seq*/rec cnsqs)))
      ((token-case lookaheads cnsqs)
       (token-case lookaheads
                   (map cr/term-seq*/rec cnsqs)))
      ((go target args)
       (go (cr/lbl-use target t lbl-e) (map cr/rhs args)))
      (_ (error 'cr/insn* "did you add a new insn*? ~a" i)))))

(define (cr/term-seq seq reg-e lbl-e)
  (if (empty? seq)
      empty
      (let ((updated-term (cr/term (first seq) reg-e lbl-e)))
        (cons updated-term
              (cr/term-seq (rest seq)
                           (add-new-reg-bindings-from-term updated-term reg-e)
                           lbl-e)))))

(define (cr/term-seq* seq reg-e lbl-e)
  (cond [(empty? seq) (error 'configure-registers "term-seq* should always end in an insn*")]
        [(empty? (rest seq)) (list (cr/term* (first seq) reg-e lbl-e))]
        [else
         (let ((updated-term (cr/term (first seq) reg-e lbl-e)))
           (cons updated-term
                 (cr/term-seq* (rest seq)
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

(define (cr/rhs rhs t reg-e)
  (if (enh:register? rhs)
      (cr/reg-use rhs t reg-e)
      rhs))

(define (cr/reg-def r t reg-e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (next-id) t u))))

(define (cr/reg-use r t reg-e)
  (match r
    ((enh:register name _ b u)
     (let ((reg (lookup-env reg-e name)))
       (enh:register-add-use! reg t)
       reg))))

(define (cr/lbl-join-point lbl t lbl-e)
  (match lbl
    ((enh:label-name name _ _ _)
     (let ((label (lookup-env lbl-e name)))
       (enh:set-label-name-binding! label t)
       label))))

(define (cr/lbl-alloc r t lbl-e)
  (match r
    ((enh:label-name name _ b u)
     (enh:label-name name (next-id) b u))))

(define (cr/lbl-use r t lbl-e)
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
