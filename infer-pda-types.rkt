#lang racket
(require "pda-data.rkt"
         "graph-algo.rkt")
(provide infer-pda-types)


;; infer-pda-types : PDA -> PDA
(define (infer-pda-types untyped-pda)
  (infer-rule-types (infer-state-types untyped-pda)))

;; infer-rule-types : PDA -> PDA
;; this only works if the states have type annotations
(define (infer-rule-types partly-untyped-pda)
  (let* ((states (pda-states partly-untyped-pda))
         (types (cleanup-rule-types (foldl gather-rule-types-from-state
                                            (hasheq)
                                            states))))
    (pda-update-rules (map (lambda (r)
                             (rule-update-stack-type (dict-ref types
                                                               (rule-name r))
                                                     r))
                           (pda-rules partly-untyped-pda))
                      partly-untyped-pda)))

;; infer-state-types : PDA -> PDA
(define (infer-state-types untyped-pda)
  (let* ((states (pda-states untyped-pda))
         (nodes (map state-name states))
         (edges (map get-edges states))
         (types (assign-types (make-hash (map list* nodes edges))
                              (pda-start untyped-pda))))
    (pda-update-states (map (lambda (st)
                              (state-update-stack-type
                               (get-stack-type types (state-name st))
                               st))
                            states)
                       untyped-pda)))

;; get-edges/state : State -> [ListOf [Pair Symbol Symbol]]
;; gathers all the possible destinations reachable in one step from this state
;; and produces a list of pairs of those destinations and their respective
;; gaurds
(define (get-edges st)
  (let ((stack-acts (append (state-non-gotos st) (state-gotos st))))
    (foldl (lambda (act ls)
             (match act
               ((shift (list token) target)
                (cons (list target token) ls))
               ((goto nt target)
                (cons (list target nt) ls))
               (_ ls)))
           '()
           stack-acts)))

;; get-stack-type : [Dict Symbol StackType] Symbol -> StackType
;; this is a simple helper function to deal with unreachable states
(define (get-stack-type types name)
  (if (dict-has-key? types name)
      (dict-ref types name #f)
      (begin (printf "State ~a is unreachable, assuming empty stack type.\n"
                     name)
             '(()))))

;; [Dict Symbol [ListOf StackType]] -> [Dict Symbol StackType]
(define (cleanup-rule-types dict)
  (for/hash ([rule (in-dict-keys dict)])
            (let ((types (dict-ref dict rule)))
             (values rule (foldl union-stack-sets
                                 (first types)
                                 (rest types))))))

(define (gather-rule-types-from-state st dict)
  (match st
    ((state name stack-type non-gotos eos-actions gotos)
     (foldl (lambda (act dict)
              (match act
                ((reduce lookahead rule)
                 (dict-cons dict rule stack-type))
                (_ dict)))
            dict
            (append non-gotos eos-actions)))))

(define (dict-cons dict key new-value)
  (dict-set dict key (cons new-value (dict-ref dict key '()))))
