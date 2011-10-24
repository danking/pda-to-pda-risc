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
;; a Reduce-To-Table is a [Dict Non-Terminal
;;                              [Dict [Syntax Identifier]
;;                                    [Syntax Identifier]]]


;; compile-pda : PDA -> PDA-RISC
(define (compile-pda a-pda)
  (match a-pda
    ((pda tokens eos start states rules)
     (let ((rto-table (gather-rto-table states)))
       (make-pdarisc
        (list (make-label
               (append (map (lambda (s)
                              (make-label-polynym (state-name s)
                                                  'unknown))
                            states)
                       (map (lambda (s)
                              (make-label-polynym (state-name s)
                                                  'have-token))
                            states)
                       (map (lambda (s)
                              (make-label-polynym (state-name s)
                                                  'eos))
                            states)
                       (map (lambda (r)
                              (make-label-polynym (rule-name r)
                                                  'have-token))
                            rules)
                       (map (lambda (r)
                              (make-label-polynym (rule-name r)
                                                  'eos))
                            rules))
               (append (map state-stack-type states)
                       (map state-stack-type states)
                       (map state-stack-type states)
                       (map rule-stack-type  rules)
                       (map rule-stack-type  rules))
               (append (map (lambda (x) #f) states) ; current-token type
                       (map (lambda (x) #f) states)
                       (map (lambda (x) #f) states)
                       (map (lambda (x) #f) rules)
                       (map (lambda (x) #f) rules))
               (append (map (lambda (x) '()) states) ; label arguments
                       (map (lambda (x) '()) states)
                       (map (lambda (x) '()) states)
                       (map (lambda (x) '()) rules)
                       (map (lambda (x) '()) rules))
               (append (map (compose list compile-state) states)
                       (map (compose list compile-have-token-state) states)
                       (map (compose list compile-eos-state) states)
                       (map (lambda (x) (list (compile-rule x
                                                            'have-token
                                                            rto-table)))
                            rules)
                       (map (lambda (x) (list (compile-rule x
                                                            'eos
                                                            rto-table)))
                            rules))
               (list (make-go (make-label-polynym start 'unknown) '())))))))))

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
    ((state name stype token-actions eos-actions gotos)
     (make-block*
      (list
       (make-if-eos
        (make-go (make-label-polynym name 'eos) '())
        (make-block*
         (list
          (make-get-token)
          (make-go (make-label-polynym name 'have-token) '())))))))))

;; compile-have-token-state : State -> Insn*
;; produces an insn* that encapsulates the behavior of the given PDA state when
;; the PDA-RISC has a token from the stream.
(define (compile-have-token-state st)
  (match st
    ((state name stype token-actions eos-actions gotos)
     (make-block*
      (list
       (make-push (make-risc-state name))
       (make-token-case
        (map (lambda (x)
               (if (empty? (action-lookahead x))
                   #f
                   (first (action-lookahead x))))
             token-actions)
        (map (lambda (x) (compile-non-eos-action x name))
             token-actions)))))))

;; compile-eos-state : State -> Insn*
;; produces an insn* that encapsulates the behavior of the given PDA state when
;; the stream is empty.
(define (compile-eos-state st)
  (match st
    ((state name stype token-actions eos-actions gotos)
     (make-block*
      (cons (make-push (make-risc-state name))
            (maybe-compile-eos-action (append (filter action-has-no-lookahead?
                                                      token-actions)
                                              eos-actions)
                                      name))))))

;; compile-rule : Rule Symbol Reduce-To-Table -> Insn*
;; Produces an insn* which encapsulates the behavior of the given rule, assuming
;; the stream is in the given state.
(define (compile-rule r stream-state rto-table)
  (match r
    ((rule name stype nt args sem-act)
     (let ((rto-states (dict-ref rto-table nt)))
       (make-block*
        `(,@(compile-rule-args args)
          ,(make-sem-act name
                         (map make-named-reg (filter (lambda (x) x) args))
                         (list (make-named-reg #'ret-val))
                         sem-act)
          ,(make-push (make-named-reg #'target))
          ,(make-push (make-named-reg #'ret-val))
          ,(make-state-case (make-named-reg #'target)
                            (map make-risc-state (dict-keys rto-states))
                            (map (lambda (target)
                                   (list (make-go
                                          (make-label-polynym target
                                                              stream-state)
                                          '())))
                                 (dict-values rto-states)))))))))

;; compile-rule-args : [ListOf Symbol] -> [ListOf Insn]
(define (compile-rule-args args)
  (if (empty? args)
      (list (make-assign (make-named-reg #'target) (make-pop)))
      (cons ; pop the state which called for the reduce
            (make-assign (make-nameless-reg) (make-pop))
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
                         (make-assign (make-named-reg #'target) (make-pop)))
                   (rest args)))))


;; compile-eos-action : Action Symbol -> [ListOf Insn*]
(define (compile-eos-action a curr-state)
  (compile-action a curr-state #t))

;; compile-non-eos-action : Action Symbol -> [ListOf Insn*]
(define (compile-non-eos-action a curr-state)
  (compile-action a curr-state #f))

;; compile-action : Action Symbol Boolean -> [ListOf Insn*]
;; If eos? is true, make all go branches branch to EOS states; otherwise,
;; assume these actions are called from the have-token state and branch
;; to the appropriate labels.
(define (compile-action a curr-state eos?)
  (match a
    ((shift l st)
     (if eos?
         (error 'compile-action
                "cannot compile a shift as an eos action")

         (list (make-push (make-curr-token #f))
               (make-drop-token)
               (make-go (make-label-polynym st 'unknown)
                        '()))))
    ((reduce l st)
     (list (make-go (make-label-polynym st (if eos?
                                               'eos
                                               'have-token)) '())))
    ((accept l)
     (list (make-assign (make-nameless-reg) (make-pop))
           (make-assign (make-named-reg #'result) (make-pop))
           (make-risc-accept (list (make-named-reg #'result)))))))

;; maybe-compile-action : [ListOf Action] -> [ListOf Insn*]
;; This first checks if the list of actions is empty, if not it compiles
;; the first action, as an eos action, and returns it. If the list of actions
;; is empty, it returns a error-halt state.
(define (maybe-compile-eos-action actions curr-state)
  (if (empty? actions)
      (list (make-reject))
      (compile-eos-action (first actions) curr-state)))

;; action-has-no-lookahead? : Action -> Boolean
;; determines if the given action has no lookahead, i.e. the action can be taken
;; when the stream is at EOS
(define (action-has-no-lookahead? act)
  (empty? (action-lookahead act)))
