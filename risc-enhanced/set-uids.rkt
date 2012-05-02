#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         "../../racket-utils/environment.rkt")

(provide configure-registers/pdarisc reset-id)

(define identity (lambda (x) x))

(define (extend-env/regs e rs)
  (extend-env/list e
                   (map enh:register-lexical-name rs)
                   (map enh:register-uid rs)))
(define (extend-env/labels e ls)
  (extend-env/list e
                   (map enh:label-name-lexical-name ls)
                   (map enh:label-name-uid ls)))

(define (configure-registers/pdarisc pr)
  (match pr
    ((pdarisc seq) (pdarisc (cr/term-seq* seq empty-env empty-env)))))

(define (cr/term-seq* seq reg-e lbl-e)
  (cond [(empty? seq) (error 'configure-registers "term-seq* should always end in an insn*")]
        [(empty? (rest seq)) (list (cr/term* (first seq) reg-e lbl-e))]
        [else
         (let ((updated-term (cr/term (first seq) reg-e)))
           (cons updated-term
                 (cr/term-seq* (rest seq)
                               (add-new-reg-bindings-from-term updated-term reg-e)
                               lbl-e)))]))

(define (cr/insn i reg-e)
  (let ((cr/reg-def (lambda (r) (cr/reg-def r reg-e)))
        (cr/reg-use (lambda (r) (cr/reg-use r reg-e)))
        (cr/rhs (lambda (r) (cr/rhs r reg-e)))
        (cr/insn (lambda (i) (cr/insn i reg-e))))
    (match-insn/recur cr/insn cr/reg-def cr/reg-use cr/rhs i)))
(define cr/term (enh:raise-to-term cr/insn))

(define (cr/insn* i reg-e lbl-e)
  (let ((cr/reg-def (lambda (r) (cr/reg-def r reg-e)))
        (cr/reg-use (lambda (r) (cr/reg-use r reg-e)))
        (cr/rhs     (lambda (r) (cr/rhs r reg-e)))
        (cr/lbl-def (lambda (l) (cr/lbl-def l lbl-e)))
        (cr/lbl-use (lambda (l) (cr/lbl-use l lbl-e))))
    (match-insn*/recur
     #:insn (lambda (i) (cr/insn i reg-e)) #:insn* (lambda (i) (cr/insn* i reg-e))
     #:regdef cr/reg-def #:reguse cr/reg-use #:rhs cr/rhs
     #:labeluse cr/lbl-use #:labeldef cr/lbl-def
     i
     [(label ids st tt
             param-lists bodies body)
      (let* ((updated-param-lists (map (lambda (param-list)
                                        (map cr/reg-def param-list))
                                      param-lists))
             (updated-labels (map cr/lbl-def ids))
             (updated-label-env (extend-env/labels lbl-e updated-labels)))
        (label updated-labels
               st tt
               updated-param-lists
               (for/list [(params updated-param-lists)
                          (body bodies)]
                 (cr/term-seq* body
                               (extend-env/regs reg-e params)
                               updated-label-env))
               (cr/term-seq* body reg-e updated-label-env)))])))
(define cr/term* (enh:raise-to-term cr/insn*))

(define (add-new-reg-bindings-from-insn i reg-e)
  (match i
    ((assign reg _)
     (extend-env reg-e
                 (enh:register-lexical-name reg)
                 (enh:register-uid reg)))
    ((sem-act _ _ regs _)
     (extend-env/regs reg-e regs))
    (_ reg-e)))
(define add-new-reg-bindings-from-term (enh:raise-input-to-term add-new-reg-bindings-from-insn))

(define-values (next-id reset-id)
  (let ((id -1))
    (values (lambda ()
              (set! id (add1 id))
              id)
            (lambda ()
              (set! id -1)))))

(define (cr/rhs rhs reg-e)
  (if (enh:register? rhs)
      (cr/reg-use rhs reg-e)
      rhs))

(define (cr/reg-def r reg-e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (next-id) b u))))

(define (cr/reg-use r reg-e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (lookup-env reg-e name) b u))))

(define (cr/lbl-def r lbl-e)
  (match r
    ((enh:label-name name _ b u)
     (enh:label-name name (next-id) b u))))

(define (cr/lbl-use r lbl-e)
  (match r
    ((enh:label-name name _ b u)
     (enh:label-name name (lookup-env lbl-e name) b u))))
