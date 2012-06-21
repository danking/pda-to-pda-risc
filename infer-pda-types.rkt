#lang racket
(require "pda-data.rkt"
         "graph-algo.rkt")
(provide infer-pda-types)


;; infer-pda-types : PDA -> PDA
(define (infer-pda-types untyped-pda)
  untyped-pda
  #;
  (infer-rule-types (infer-state-types untyped-pda)))

;; infer-rule-types : PDA -> PDA
;; this only works if the states have type annotations
(define (infer-rule-types partly-untyped-pda)
  (let* ((states (pda-states partly-untyped-pda))
         (types (cleanup-rule-types (foldl gather-rule-types-from-state
                                           (hasheq)
                                           states))))
    (pda-update-rules (map (lambda (r)
                             (rule-update-stack-type
                              (get-stack-type types
                                              (syntax-e (rule-name r)))
                              r))
                           (pda-rules partly-untyped-pda))
                      partly-untyped-pda)))

;; infer-state-types : PDA -> PDA
(define (infer-state-types untyped-pda)
  (let* ((states (pda-states untyped-pda))
         (nodes (map (compose syntax-e state-name) states))
         (edges (map get-edges states))
         (types (infer-stack-types (make-hash (map list* nodes edges))
                                   (syntax-e (pda-start untyped-pda)))))
    (pda-update-states (map (lambda (st)
                              (state-update-stack-type
                               (get-stack-type types (syntax-e (state-name st)))
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
                (cons (list (syntax-e target)
                            (syntax-e token))
                      ls))
               ((goto nt target)
                (cons (list (syntax-e target)
                            nt)
                      ls))
               (_ ls)))
           '()
           stack-acts)))

;; get-stack-type : [Dict Symbol StackType] Symbol -> StackType
;; this is a simple helper function to deal with unreachable states
(define (get-stack-type types name)
  (if (dict-has-key? types name)
      (dict-ref types name #f)
      (begin (printf "~a is unreachable, assigning bottom.\n"
                     name)
             'bottom)))

;; cleanup-rule-types : [Dict Symbol [ListOf StackType]]
;;                      ->
;;                      [Dict Symbol StackType]
(define (cleanup-rule-types dict)
  (for/hash ([(rule types) dict])
    (values rule (foldl union-stack-sets
                        (first types)
                        (rest types)))))

;; union-stack-sets : [ListOf Stack] [ListOf Stack] -> [ListOf Stack]
;; computes the union of two sets of stacks
;; FIXME: PDAs should carry around sets instead of these stupid lists
;;        until I take the time tor rework a huge portion of the
;;        compiler we'll deal with this wart here that replicates
;;        behavior from the node-graph.rkt module
(require (only-in srfi/1 lset-union))
(define (union-stack-sets s1 s2)
  (define (match-lengths-in-sets s1 s2)
    (let ((l1 (length (first s1)))
          (l2 (length (first s2))))
      (cond [(< l1 l2) (values s1 (truncate-to s2 l1))]
            [(> l1 l2) (values (truncate-to s1 l2) s2)]
            [else (values s1 s2)])))
  (define (truncate-to los n)
    (let loop ((los los)
               (n n)
               (acc (map (lambda (x) '()) los)))
      (if (zero? n)
          (map reverse acc)
          (loop (map rest los)
                (sub1 n)
                (map cons
                     (map first los)
                     acc)))))

  (let-values (((s1 s2)
                (match-lengths-in-sets s1 s2)))
    (lset-union equal? s1 s2)))


;; gather-rule-types-from-state : State
;;                                [Dict Symbol [ListOf StackType]]
;;                                ->
;;                                [Dict Symbol [ListOf StackType]]
(define (gather-rule-types-from-state st dict)
  (match st
    ((state name stack-type non-gotos eos-actions gotos)
     (foldl (lambda (act dict)
              (match act
                ((reduce lookahead rule)
                 (dict-cons dict
                            (syntax-e rule)
                            ; The name is pushed before control transfers to the
                            ; reduction. This is necessary to handle empty rule
                            ; productions.
                            (map (lambda (stack)
                                   (cons (syntax-e name) stack))
                                 stack-type)))
                (_ dict)))
            dict
            (append non-gotos eos-actions)))))

(define (dict-cons dict key new-value)
  (dict-set dict key (cons new-value (dict-ref dict key '()))))
