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
               (append (map (compose make-label-name state-name) states)
                       (map (lambda (s)
                              (make-label-polynym (state-name s)
                                                  'have-token))
                            states)
                       (map (compose make-label-name rule-name)  rules))
               (append (map state-stack-type states)
                       (map state-stack-type states)
                       (map rule-stack-type  rules))
               (append (map (lambda (x) #f) states) ; current-token type
                       (map (lambda (x) #f) states)
                       (map (lambda (x) #f) rules))
               (append (map (lambda (x) '()) states) ; label arguments
                       (map (lambda (x) '()) states)
                       (map (lambda (x) '()) rules))
               (append (map (compose list compile-state) states)
                       (map (compose list compile-have-token-state) states)
                       (map (lambda (x) (list (compile-rule x rto-table)))
                            rules))
               (list (make-go (make-label-name start) '())))))))))

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
        (make-risc-accept (list (make-named-reg 'reject)))
        (make-block*
         (list
          (make-get-token)
          (make-go (make-label-polynym name 'have-token) '())))))))))

;; compile-have-token-state : State -> Insn*
;; produces an insn* that encapsulates the behavior of the given PDA state when
;; the PDA-RISC has a token from the stream.
(define (compile-have-token-state st)
  (match st
    ((state name stype token-actions gotos)
     (make-block*
      (list
       (make-token-case
        (map (lambda (x)
               (if (empty? (action-lookahead x))
                   #t
                   (first (action-lookahead x))))
             token-actions)
        (map (lambda (x) (compile-action x name))
             token-actions)))))))

;; compile-rule : Rule Reduce-To-Table -> Insn*
(define (compile-rule r rto-table)
  (match r
    ((rule name stype nt args sem-act)
     (let ((rto-states (dict-ref rto-table nt)))
       (make-block*
        `(,@(compile-rule-args args)
          ,(make-sem-act (map make-named-reg (filter (lambda (x) x) args))
                         (list (make-named-reg 'ret-val))
                         sem-act)
          ,(make-push (make-named-reg 'target))
          ,(make-push (make-named-reg 'ret-val))
          ,(make-state-case (make-named-reg 'target)
                            (map make-risc-state (dict-keys rto-states))
                            (map (lambda (target)
                                   (list (make-go
                                          (make-label-polynym target
                                                              'have-token)
                                          '())))
                                 (dict-values rto-states)))))))))


;; compile-rule-args : [ListOf Symbol] -> [ListOf Insn]
(define (compile-rule-args args)
  (if (empty? args)
      (list (make-assign (make-named-reg 'v) (make-pop))
            (make-assign (make-named-reg 'target) (make-pop))
            (make-push (make-named-reg 'v))
            (make-push (make-named-reg 'target)))
      (foldl (lambda (x xs)
               (if x
                   (list* (make-assign (make-named-reg x) (make-pop))
                          (make-assign (make-nameless-reg) (make-pop))
                          xs)
                   (list* (make-assign (make-nameless-reg) (make-pop))
                          (make-assign (make-nameless-reg) (make-pop))
                          xs)))
             (list (make-assign (if (first args)
                                    (make-named-reg (first args))
                                    (make-nameless-reg))
                                (make-pop))
                   (make-assign (make-named-reg 'target) (make-pop)))
             (rest args))))


;; compile-action : Action Symbol -> [ListOf Insn*]
(define (compile-action a curr-state)
  (match a
    ((shift l st)
     (list (make-push (make-risc-state curr-state))
           (make-push (make-curr-token #f))
           (make-drop-token)
           (make-go (make-label-polynym st 'unknown) '())))
    ((reduce l st)
     (list (make-go (make-label-polynym st 'have-token) '())))
    ((goto nt st)
     (list (make-push (make-risc-state curr-state))
           (make-push (make-nterm nt))
           (make-go (make-label-polynym st 'have-token) '())))
    ((accept l)
     (list (make-risc-accept '())))))

;; action-has-no-lookahead? : Action -> Boolean
;; determines if the given action has no lookahead, i.e. the action can be taken
;; when the stream is at EOS
(define (action-has-no-lookahead? act)
  (empty? (action-lookahead act)))
