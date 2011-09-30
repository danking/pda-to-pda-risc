#lang racket
(require "pda-data.rkt"
         (rename-in "pdarisc-data.rkt"
                    [struct:state struct:risc-state]
                    [state? risc-state?]
                    [state risc-state]
                    [make-state make-risc-state]
                    [state-id risc-state-id]
                    [struct:accept struct:risc-accept]
                    [accept? risc-accept?]
                    [accept risc-accept]
                    [make-accept make-risc-accept]
                    [accept-vals risc-accept-vals])
         "parse-pda.rkt"
         rackunit)
(provide compile-pda)

(define make-dict hasheq)
;; a Reduce-To-Table is a [Dict Non-Terminal [Dict State-Name State-Name]]


;; compile-pda : PDA -> PDA-RISC
(define (compile-pda a-pda)
  (match a-pda
    ((pda tokens eos start states rules)
     (let ((rto-table (gather-rto-table states)))
       (make-pdarisc
        (list (make-label
               (append (map state-name states)
                       (map rule-name  rules))
               (append (map state-stack-type states)
                       (map rule-stack-type  rules))
               (append (map (lambda (x) #f) states)
                       (map (lambda (x) #f) rules))
               (append (map (lambda (x) '()) states)
                       (map (lambda (x) '()) rules))
               (append (map (lambda (x) (list (compile-state x)))
                            states)
                       (map (lambda (x) (list (compile-rule x rto-table)))
                            rules))
               (list (make-go start '())))))))))

;; gather-rto-table : [ListOf State] -> Reduce-To-Table
(define (gather-rto-table states)
  (foldl (lambda (gotos s table)
           (foldl (lambda (g t)
                    (add-reduce-to-pair g s t))
                  table
                  gotos))
         (make-dict)
         (map state-gotos states)
         (map state-name states)))

;; add-reduce-to-pair : Goto Symbol Reduce-To-Table -> Reduce-To-Table
;; reduce-to-pair will add the implicit Reduce-To-Table entry from the goto
(define (add-reduce-to-pair a-goto curr-st table)
  (match a-goto
    ((goto nt dest-st)
     (dict-update table nt (lambda (st-map)
                             (dict-set st-map curr-st dest-st))
                  (make-dict)))))

;; compile-state : State -> Insn*
(define (compile-state st)
  (match st
    ((state name stype token-actions gotos)
     (make-block*
      (list
       (make-if-eos
        (make-risc-accept '(reject))
        (make-block*
         (list
          (make-get-token)
          (make-token-case
           (map (lambda (x)
                  (if (empty? (action-lookahead x))
                      #t
                      (first (action-lookahead x))))
                token-actions)
           (map (lambda (x) (compile-action x name))
                token-actions))))))))))


;; compile-rule : Rule Reduce-To-Table -> Insn*
(define (compile-rule r rto-table)
  (match r
    ((rule name stype nt args sem-act)
     (let ((rto-states (dict-ref rto-table nt)))
       (make-block*
        `(,@(compile-rule-args args)
          ,(make-sem-act (filter (lambda (x) x) args) '(ret-val) sem-act)
          ,(make-push (make-var-ref 'target))
          ,(make-push (make-var-ref 'ret-val))
          ,(make-state-case (make-var-ref 'target)
                            (dict-keys rto-states)
                            (map (lambda (target) (list (make-go target '())))
                                 (dict-values rto-states)))))))))


;; compile-rule-args : [ListOf Symbol] -> [ListOf Insn]
(define (compile-rule-args args)
  (if (empty? args)
      (list (make-assign 'v (make-pop))
            (make-assign 'target (make-pop))
            (make-push (make-var-ref 'v))
            (make-push (make-var-ref 'target)))
      (foldl (lambda (x xs)
               (if x
                   (list* (make-assign x (make-pop))
                          (make-assign '_ (make-pop))
                          xs)
                   (list* (make-assign '_ (make-pop))
                          (make-assign '_ (make-pop))
                          xs)))
             (list (make-assign (first args) (make-pop))
                   (make-assign 'target (make-pop)))
             (rest args))))


;; compile-action : Action Symbol -> Insn*
(define (compile-action a curr-state)
  (match a
    ((shift l st)
     (make-block* (list (make-push (make-risc-state curr-state))
                        (make-push (make-curr-token 0))
                        (make-drop-token)
                        (make-go st '()))))
    ((reduce l st)
     (make-block* (list (make-drop-token)
                        (make-go st '()))))
    ((goto nt st)
     (make-block* (list (make-push (make-risc-state curr-state))
                        (make-push (make-nterm nt))
                        (make-go st '()))))
    ((accept l)
     (make-block* (list (make-risc-accept '()))))))


