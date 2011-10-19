#lang racket
(require syntax/parse
         "pda-syntax-literals.rkt"
         (prefix-in pda- "pda-data.rkt"))
(provide pda-literals untyped-pda-clauses-stx typed-pda-clauses-stx
         EOS START TOKENS COMMENT STATE RULE
         SHIFT REDUCE ACCEPT)

(define-literal-set pda-literals
  (EOS START TOKENS COMMENT STATE RULE))

(define-literal-set action-literals
  (SHIFT REDUCE ACCEPT))

(define-syntax define-pda-clauses-stx
  (syntax-rules ()
    ((_ name state-stx rule-stx)
     (define-syntax-class name
       #:description "a series of pda clauses"
       #:attributes (compiled)
       #:literal-sets ()
       (pattern ((~var head state-stx) . (~var rest name))
                #:attr compiled (pda-add-state (attribute head.compiled)
                                               (attribute rest.compiled)))
       (pattern ((~var head rule-stx) . (~var rest name))
                #:attr compiled (pda-add-rule (attribute head.compiled)
                                              (attribute rest.compiled)))
       (pattern (head:eos-stx . (~var rest name))
                #:attr compiled (pda-pda-update-eos #'head.compiled
                                                    (attribute rest.compiled)))
       (pattern (head:start-stx . (~var rest name))
                #:attr compiled (pda-pda-update-start
                                 #'head.compiled
                                 (attribute rest.compiled)))
       (pattern (head:token-set-stx . (~var rest name))
                #:attr compiled (pda-pda-update-tokens
                                 (attribute head.compiled)
                                 (attribute rest.compiled)))
       (pattern (head:comment-stx . (~var rest name))
                #:attr compiled (attribute rest.compiled))
       (pattern () #:attr compiled pda-empty-pda)))))

(define-pda-clauses-stx untyped-pda-clauses-stx
  untyped-state-stx untyped-rule-stx)

(define-pda-clauses-stx typed-pda-clauses-stx
  typed-state-stx typed-rule-stx)

(define-syntax-class untyped-state-stx
  #:description "a pda state"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (STATE name:identifier (~or _:comment-stx
                                       action:non-goto-action-stx
                                       goto:goto-stx) ...)
           #:attr compiled (pda-make-state
                            #'name
                            #f
                            (attribute action.compiled)
                            (list)
                            (attribute goto.compiled))))

(define-syntax-class untyped-rule-stx
  #:description "a pda rule"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (RULE name:identifier non-terminal:identifier
                 (bindings:maybe-id ...)
                 sem-act)
           #:attr compiled (pda-make-rule #'name
                                          #f
                                          (syntax-e #'non-terminal)
                                          (map (lambda (x)
                                                 (if (false? (syntax-e x))
                                                     #f
                                                     x))
                                               (syntax-e #'(bindings ...)))
                                          #'sem-act)))

(define-syntax-class typed-state-stx
  #:description "a pda state"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (STATE name:identifier (~datum :) st:stack-type-stx
                  (~or _:comment-stx
                       action:non-goto-action-stx
                       goto:goto-stx) ...)
           #:attr compiled (pda-make-state
                            #'name
                            (attribute st.compiled)
                            (attribute action.compiled)
                            (list)
                            (attribute goto.compiled))))

(define-syntax-class typed-rule-stx
  #:description "a pda rule"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (RULE name:identifier (~datum :) st:stack-type-stx
                 non-terminal:identifier
                 (bindings:maybe-id ...)
                 sem-act)
           #:attr compiled (pda-make-rule #'name
                                          (attribute st.compiled)
                                          (syntax-e #'non-terminal)
                                          (map (lambda (x)
                                                 (if (false? (syntax-e x))
                                                     #f
                                                     x))
                                               (syntax-e #'(bindings ...)))
                                          #'sem-act)))

(define-syntax-class eos-stx
  #:description "a pda eos declaration"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (EOS token:identifier)
           #:attr compiled #'token))

(define-syntax-class token-set-stx
  #:description "a pda token-set declaration"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (TOKENS token:identifier ...)
           #:attr compiled (syntax->list #'(token ...))))

(define-syntax-class start-stx
  #:description "a pda start declaration"
  #:attributes (compiled)
  #:literal-sets (pda-literals)
  (pattern (START state:identifier)
           #:attr compiled #'state))

(define-syntax-class comment-stx
  #:description "a pda comment"
  #:literal-sets (pda-literals)
  (pattern (COMMENT any ...)))

(define-syntax-class non-goto-action-stx
  #:attributes (compiled)
  (pattern a:accept-stx
           #:attr compiled (attribute a.compiled))
  (pattern r:reduce-stx
           #:attr compiled (attribute r.compiled))
  (pattern s:shift-stx
           #:attr compiled (attribute s.compiled)))

(define-syntax-class shift-stx
  #:description "a shift action"
  #:attributes (compiled)
  #:literal-sets (action-literals)
  (pattern (SHIFT lookahead target)
           #:attr compiled (pda-make-shift (syntax-e #'lookahead)
                                           #'target)))

(define-syntax-class reduce-stx
  #:description "a reduce action"
  #:attributes (compiled)
  #:literal-sets (action-literals)
  (pattern (REDUCE lookahead target)
           #:attr compiled (pda-make-reduce (syntax-e #'lookahead)
                                            #'target)))

(define-syntax-class goto-stx
  #:description "an accept action"
  #:attributes (compiled)
  #:literal-sets (action-literals)
  (pattern (GOTO lookahead target)
           #:attr compiled (pda-make-goto (syntax-e #'lookahead)
                                          #'target)))

(define-syntax-class accept-stx
  #:description "an accept action"
  #:attributes (compiled)
  #:literal-sets (action-literals)
  (pattern (ACCEPT lookahead)
           #:attr compiled (pda-make-accept (syntax-e #'lookahead))))

(define-syntax-class maybe-id
  #:description "an identifier or #f"
  (pattern (~or boolean identifier)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; StackTypes

(define-syntax-class stack-type-stx
  #:description "a stack type"
  #:attributes (compiled)
  (pattern (sc:stack-configuration-stx ...)
           #:attr compiled (attribute sc.compiled)))

(define-syntax-class stack-configuration-stx
  #:description "a stack configuration"
  #:attributes (compiled)
  (pattern (stack-element:identifier ...)
           #:attr compiled (syntax->list #'(stack-element ...))))

(define (unsyntax-action-list x)
  (map syntax-e (syntax->list x)))

(define (pda-add-state new-state pda)
  (pda-pda-update-states (cons new-state (pda-pda-states pda))
                         pda))

(define (pda-add-rule new-rule pda)
  (pda-pda-update-rules (cons new-rule (pda-pda-rules pda))
                         pda))
