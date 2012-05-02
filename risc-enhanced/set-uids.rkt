#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt")

(provide configure-registers/pdarisc reset-id)

(define identity (lambda (x) x))

(define empty-env (hash))
(define extend-env hash-set)
(define lookup-env hash-ref)
(define (extend-env/regs e rs)
  (for/fold [(e e)]
            [(r rs) #:when r]
    (extend-env e
                (enh:register-lexical-name r)
                (enh:register-uid r))))

(define (raise-to-term f)
  (lambda (t . rest)
    (match t
      ((enh:pda-term a b c d i)
       (enh:pda-term a b c d (apply f (cons i rest)))))))

(define (raise-input-to-term f)
  (lambda (t . rest)
    (match t
      ((enh:pda-term a b c d i)
       (apply f (cons i rest))))))

;; maybe-f : [A -> B] -> [(U A #f) -> (U B #f)]
(define (maybe-f f)
  (lambda (n)
    (if n (f n) n)))

(define-values (next-id reset-id)
  (let ((id -1))
    (values (lambda ()
              (set! id (add1 id))
              id)
            (lambda ()
              (set! id -1)))))

(define (configure-registers/pdarisc pr)
  (match pr
    ((pdarisc seq) (pdarisc (cr/term-seq* seq empty-env)))))

(define (cr/term-seq* seq e)
  (cond [(empty? seq) (error 'configure-registers "term-seq* should always end in an insn*")]
        [(empty? (rest seq)) (list (cr/term* (first seq) e))]
        [else
         (let ((updated-term (cr/term (first seq) e)))
           (cons updated-term
                 (cr/term-seq* (rest seq)
                               (add-new-bindings-from-term updated-term e))))]))

(define (cr/insn i e)
  (let ((cr/reg-def (lambda (r) (cr/reg-def r e)))
        (cr/reg-use (lambda (r) (cr/reg-use r e)))
        (cr/rhs (lambda (r) (cr/rhs r e)))
        (cr/insn (lambda (i) (cr/insn i e))))
    (match-insn/recur cr/insn cr/reg-def cr/reg-use cr/rhs i)))

(define cr/term (raise-to-term cr/insn))

(define (cr/insn* i e)
  (let ((cr/reg-def (lambda (r) (cr/reg-def r e)))
        (cr/reg-use (lambda (r) (cr/reg-use r e)))
        (cr/rhs (lambda (r) (cr/rhs r e))))
    (match-insn*/recur
     #:insn (lambda (i) (cr/insn i e)) #:insn* (lambda (i) (cr/insn* i e))
     #:regdef cr/reg-def #:reguse cr/reg-use #:rhs cr/rhs
     i
     [(label ids st tt
             param-lists bodies body)
      (let ((updated-param-lists (map (lambda (param-list)
                                        (map cr/reg-def param-list))
                                      param-lists)))
        (label ids
               st tt
               updated-param-lists
               (for/list [(params updated-param-lists)
                          (body bodies)]
                 (cr/term-seq* body (extend-env/regs e params)))
               (cr/term-seq* body e)))])))

(define cr/term* (raise-to-term cr/insn*))

(define (add-new-bindings-from-insn i e)
  (match i
    ((assign reg _)
     (extend-env e
                 (enh:register-lexical-name reg)
                 (enh:register-uid reg)))
    ((sem-act _ _ regs _)
     (extend-env/regs e regs))
    (_ e)))

(define add-new-bindings-from-term (raise-input-to-term add-new-bindings-from-insn))

(define (cr/rhs rhs e)
  (if (enh:register? rhs)
      (cr/reg-use rhs e)
      rhs))

(define (cr/reg-def r e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (next-id) b u))))

(define (cr/reg-use r e)
  (match r
    ((enh:register name _ b u)
     (enh:register name (lookup-env e name) b u))))