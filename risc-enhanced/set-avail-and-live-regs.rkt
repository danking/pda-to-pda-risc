#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "../../racket-utils/graph-utils.rkt")
(provide set-avail/live-regs!)

(define (maybe-set->set-eq s)
  (if s s (seteq)))

(define (set-avail/live-regs! pr)
  (match pr
    ((pdarisc seq)
     (set-avail-regs/term! (seteq (first seq)))
     (set-live-regs/term! (find-sinks (seteq (first seq)) enh:pda-term-succs)))))

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
            (subset? avail-in (selector t)))))

(define (update-avail/term! t s)
  (enh:set-pda-term-avail-regs!
   t (set-union (maybe-set->set-eq (enh:pda-term-avail-regs t)) s)))
(define (update-live/term! t s)
  (enh:set-pda-term-live-regs!
   t (set-union (maybe-set->set-eq (enh:pda-term-live-regs t)) s)))


(define join-point/term? (enh:raise-input-to-term enh:join-point?))

(define (avail-out/term t)
  (if (join-point/term? t)
      (seteq)
      (let ((avail-in (maybe-set->set-eq (enh:pda-term-avail-regs t)))
            (kill-set (avail-kill-set/insn (enh:pda-term-insn t)))
            (gen-set  (avail-gen-set/insn (enh:pda-term-insn t))))
        (set-union (set-subtract avail-in kill-set) gen-set))))
(define (live-in/term t)
  (if (join-point/term? t)
      (seteq)
      (let ((live-out (maybe-set->set-eq (enh:pda-term-live-regs t)))
            (kill-set (live-kill-set/insn (enh:pda-term-insn t)))
            (gen-set  (live-gen-set/insn (enh:pda-term-insn t))))
        (set-union (set-subtract live-out kill-set) gen-set))))

(define (avail-kill-set/insn i) (reg-uses i))
(define (avail-gen-set/insn i) (reg-defs i))

(define (live-kill-set/insn i) (reg-defs i))
(define (live-gen-set/insn i) (reg-uses i))

(define (reg-uses i)
  (define (rhs-uses rhs)
    (if (enh:register? rhs) (seteq rhs) (seteq)))

  (match i
    ((assign reg rhs) (rhs-uses rhs))
    ((push rhs) (rhs-uses rhs))
    ((sem-act _ in _ _) (list->seteq in))
    ((block _) (seteq))
    ((drop-token) (seteq))
    ((get-token) (seteq))
    ((stack-ensure _) (seteq))
    ((enh:join-point _ _) (seteq))
    ((label _ _ _ _ _ _) (seteq))
    ((block* _) (seteq))
    ((accept regs) (list->seteq regs))
    ((reject) (seteq))
    ((if-eos _ _) (seteq))
    ((state-case reg _ _) (seteq reg))
    ((token-case _ _) (seteq))
    ((go _ rhs) (list->seteq (filter register? rhs)))
    (_ (error 'reg-uses "passed something other than an insn or insn*"))))
(define (reg-defs i)
  (match i
    ((assign reg rhs) (seteq reg))
    ((push rhs) (seteq))
    ((sem-act _ _ out _) (list->seteq (filter register? out)))
    ((block term-seq) (seteq))
    ((drop-token) (seteq))
    ((get-token) (seteq))
    ((stack-ensure _) (seteq))
    ((enh:join-point _ regs) (list->seteq regs))
    ((label _ _ _ _ _ _) (seteq))
    ((block* _) (seteq))
    ((accept _) (seteq))
    ((reject) (seteq))
    ((if-eos _ _) (seteq))
    ((state-case _ _ _) (seteq))
    ((token-case _ _) (seteq))
    ((go _ _) (seteq))
    (_ (error 'reg-defs "passed something other than an insn or insn*"))))