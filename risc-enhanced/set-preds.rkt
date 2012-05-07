#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt")
         "fold-enhanced.rkt"
         (prefix-in basic: "../../racket-utils/environment.rkt")
         "../../racket-utils/mutable-set.rkt")
(provide set-preds!)

(define empty-env basic:empty-env)
(define (extend-env e l v)
  (basic:extend-env e (enh:label-name-uid l) v))
(define (lookup-env e l)
  (basic:lookup-env e (enh:label-name-uid l)))

(define (set-preds! pr)
  (match pr
    ((pdarisc seq)
     (set-preds/term-seq*! seq #f empty-env))))

(define (set-preds/term-seq*! seq pred env)
  (cond [(empty? seq) (error 'term-seq* "cannot have an empty term-seq*")]
        [(empty? (rest seq)) (set-preds/term*! (first seq) pred env)]
        [else (set-preds/term! (first seq)
                               pred
                               (first (rest seq)))
              (set-preds/term-seq*! (rest seq) (first seq) env)]))

(define (set-preds/term! t pred succ)
  (when pred (set-add! (enh:pda-term-preds t) pred))
  (set-add! (enh:pda-term-succs t) succ))

(define (set-preds/term*! t pred env)
  (when pred (set-add! (enh:pda-term-preds t) pred))
  (for ((succ (get-succs/term* t env)))
    (set-add! (enh:pda-term-succs t) succ))
  (match-insn*/recur #:term set-preds/term! #:term* set-preds/term*!
                     #:term-seq* set-preds/term-seq*!
    (enh:pda-term-insn t)
    ((label ids _ _ param-lists bodies body)
     (let ((updated-label-env (for/fold ((e env))
                                        ((id ids)
                                         (b bodies))
                                (extend-env e id (first b)))))
       (for-each (lambda (b) (set-preds/term-seq*! b t updated-label-env)) bodies)
       (set-preds/term-seq*! body t updated-label-env)))
    ((go target args)
     (set-add! (enh:pda-term-preds (lookup-env env target)) t)))
  (void))

(define (get-succs/insn* i env)
  (match i
    ((label ids stack-types token-types
            param-lists bodies body)
     (cons (first body) (map first bodies)))
    ((block* insns)
     (list (first insns)))
    ((accept vals)
     (list))
    ((reject)
     (list))
    ((if-eos cnsq altr)
     (list cnsq altr))
    ((state-case st lookaheads cnsqs)
     (map first cnsqs))
    ((token-case lookaheads cnsqs)
     (map first cnsqs))
    ((go target args)
     (list (lookup-env env target)))))
(define get-succs/term* (enh:raise-input-to-term get-succs/insn*))
