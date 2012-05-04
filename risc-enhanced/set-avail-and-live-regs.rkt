#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         (prefix-in basic: "../../racket-utils/environment.rkt")
         "../../racket-utils/graph-utils.rkt"
         "../../racket-utils/set-utilities.rkt")
(provide set-avail/live-regs!)

(define (maybe-set->set s)
  (if s s (set)))

(define empty-env basic:empty-env)
(define (extend-env e l v)
  (basic:extend-env e (enh:register-uid l) v))
(define (lookup-env e l)
  (basic:lookup-env e (enh:register-uid l)))

(define (set-avail/live-regs! pr)
  (match pr
    ((pdarisc seq)
     (set-avail-regs/term! (set (first seq)))
     (set-live-regs/term! (find-sinks (set (first seq)) enh:pda-term-succs)))))

(define (set-avail-regs/term! term-set)
  (graph-fixed-point! term-set
                      enh:pda-term-succs
                      (lambda (succ curr)
                        (new-information? succ
                                          enh:pda-term-avail-regs
                                          (avail-out/term curr)))
                      (lambda (succ curr)
                        (update-avail/term! succ
                                            (avail-out/term curr)))))
(define (set-live-regs/term! term-set)
  (graph-fixed-point! term-set
                      enh:pda-term-preds
                      (lambda (pred curr)
                        (new-information? pred
                                          enh:pda-term-live-regs
                                          (live-in/term curr)))
                      (lambda (pred curr)
                        (update-live/term! pred
                                           (live-in/term curr)))))

(define (new-information? t selector avail-in)
  (not (and (selector t) ;; initialized?
            (for/and ((v avail-in))
              (set-member? (selector t) v)))))

(define (update-avail/term! t s)
  (enh:set-pda-term-avail-regs!
   t (set-union (maybe-set->set (enh:pda-term-avail-regs t)) s)))
(define (update-live/term! t s)
  (enh:set-pda-term-live-regs!
   t (set-union (maybe-set->set (enh:pda-term-live-regs t)) s)))

(define (avail-out/term t)
  (set-union (set-subtract (maybe-set->set (enh:pda-term-avail-regs t))
                           (avail-kill-set/insn (enh:pda-term-insn t)))
             (avail-gen-set/insn (enh:pda-term-insn t))))
(define (live-in/term t)
  (set-union (set-subtract (maybe-set->set (enh:pda-term-live-regs t))
                           (live-kill-set/insn (enh:pda-term-insn t)))
             (live-gen-set/insn (enh:pda-term-insn t))))

(define (avail-kill-set/insn i) (reg-uses i))
(define (avail-gen-set/insn i) (reg-defs i))

(define (live-kill-set/insn i) (reg-defs i))
(define (live-gen-set/insn i) (reg-uses i))

(define (reg-uses i)
  (define (rhs-uses rhs)
    (if (register? rhs) (set rhs) (set)))

  (match i
    ((assign reg rhs) (rhs-uses rhs))
    ((push rhs) (rhs-uses rhs))
    ((sem-act _ in _ _) (list->set in))
    ((block _) (set))
    ((drop-token) (set))
    ((get-token) (set))
    ((stack-ensure _) (set))
    ((enh:join-point _) (set))
    ((label _ _ _ _ _ _) (set))
    ((block* _) (set))
    ((accept regs) (list->set regs))
    ((reject) (set))
    ((if-eos _ _) (set))
    ((state-case reg _ _) (set reg))
    ((token-case _ _) (set))
    ((go _ rhs) (list->set (filter register? rhs)))
    (_ (error 'reg-uses "passed something other than an insn or insn*"))))
(define (reg-defs i)
  (match i
    ((assign reg rhs) (set reg))
    ((push rhs) (set))
    ((sem-act _ _ out _) (list->set (filter register? out)))
    ((block term-seq) (set))
    ((drop-token) (set))
    ((get-token) (set))
    ((stack-ensure _) (set))
    ((enh:join-point regs) (list->set regs))
    ((label _ _ _ _ _ _) (set))
    ((block* _) (set))
    ((accept _) (set))
    ((reject) (set))
    ((if-eos _ _) (set))
    ((state-case _ _ _) (set))
    ((token-case _ _) (set))
    ((go _ _) (set))
    (_ (error 'reg-defs "passed something other than an insn or insn*"))))