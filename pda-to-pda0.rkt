#lang racket
(provide convert-pda)

;; The data definitions (i.e. grammars) are after the code

;; convert-pda : [ListOf Clause] -> [ListOf Clause]
;; convert a pda description into a pda0 description
(define (convert-pda pda)
  (let* ((used-names (map second (filter state? pda)))
         (accept-block (uniqify 'accept-block used-names)))
    (foldr (lambda (clause more) (convert-pda-clause clause more accept-block))
           `((block ,accept-block #f #f accept))
           pda)))

;; dispatches to various pda-clause converters
(define (convert-pda-clause pda-clause more accept-block)
  (case (first pda-clause)
    [(state)   (append (convert-state pda-clause accept-block)
                       more)]
    [(rule)    (list* (convert-rule pda-clause accept-block)
                      pda-clause           ; include the rule itself
                      more)]
    [(tokens)  (cons pda-clause more)]
    [(comment) more]
    [else      (error 'convert-pda "unsupported pda-clause : ~a"
                      pda-clause)]))

;; produce an id, based on name, which is unique relative to used
(define (uniqify name used)
  (let loop ((name name)
             (used used))
    (if (memq name used)
        (loop (string->symbol (string-append "_" (symbol->string name)))
              used)
        name)))

(define (state? s)
  (eq? (first s) 'state))
(define (rule? r)
  (eq? (first r) 'rule))

;; produces a rule block which handles reduction, state pop'ing, and jumping
(define (convert-rule rule accept-block)
  (let ((name (second rule))
        (vars (fourth rule)))
    `(block ,name #f #f    ; blocks and rules have different namespaces (?)
            (reduce ,name)
            (pop-states ,(length vars))
            return)))

;; produce a series of pda0 blocks which represent the given pda state
(define (convert-state state accept-block)
  (let-values (((name) (second state))
               ((goto-actions other-actions) (categorize-actions (cddr state))))
    (produce-state-block name
                         goto-actions
                         other-actions
                         accept-block)))

;; group all the gotos together, must maintain order of actions per pda spec
(define (categorize-actions actions)
  (let loop ((gotos '())
             (others '())
             (actions actions))
    (cond [(empty? actions)
           (values (reverse gotos) (reverse others))]
          [(eq? (caar actions) 'goto)
           (loop (cons (car actions) gotos) others (rest actions))]
          [else
           (loop gotos (cons (car actions) others) (rest actions))])))

;; expands a state (name, goto actions, other actions) into its primary state
;; and its reduction state
(define (produce-state-block name gotos other-actions accept-block)
  (let ((body-state   (symbol-append name '-body))
        (reduce-state (symbol-append name '-reduce)))
    `((block ,name #f #f
             push-token
             (jump ,body-state))
      (block ,body-state #f #f
             (push-state ,reduce-state)
             (lcase . ,(format-non-goto-actions other-actions accept-block)))
      (block ,reduce-state #f #f
             (push-state ,reduce-state)
             (gcase . ,(format-goto-actions gotos accept-block))))))

;; Symbol Symbol -> Symbol
;; appends the string versions of the symbols to produce a new symbol
;; (symbol-append 'foo 'bar) => 'foobar
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))

;; trims off the clause type identifier and contorts accept clauses so that
;; they point to the accepting block
(define (format-non-goto-actions clauses accept-block)
  (map (lambda (x)
         (if (eq? (first x) 'accept)
             `(,(second x) ,accept-block)
             `(,(second x) ,(third x))))
       clauses))

;; similar to `format-non-goto-actions', but this changes the target states to
;; NAME-body to avoid a token push on the NAME state
(define (format-goto-actions clauses accept-block)
  (map (lambda (x)
         `(,(second x) ,(symbol-append (third x) '-body)))
       clauses))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammars

;; pda ::= (pda-clause ...)
;; pda-clause ::= (COMMENT whatever ...)
;;        | (TOKENS token ...)
;;              | (ERROR-SYMBOL ident [semantic-value-proc])
;;              | (NO-SKIP token ...)
;;              | (STATE state-name action ...)
;;              | (RULE rule-name non-term bindings semantic-action)

;; action ::= (COMMENT whatever ...)
;;          | (SHIFT  lookahead state-name)
;;          | (REDUCE lookahead rule-name)
;;          | (ACCEPT lookahead)
;;          | (GOTO non-term state-name)
;;    | (ERROR-SHIFT ident state-name)

;; lookahead ::= (token ... [#f])  ; Terminating #f means eos.
;; bindings ::= (boolean ...)

;; state-name, rule-name ::= ident
;; token, non-term ::= ident


;; pda0 ::= (clause ...)

;; clause ::= (TOKENS token ...)
;;          | (NO-SKIP token ...)
;;          | (BLOCK block-name error-handler token-count  insn ... branch)
;;          | (RULE rule-name non-term bindings semantic-action)
;;          | (COMMENT stuff ...)

;; insn ::= PUSH-TOKEN | EAT-TOKEN
;;        | (REDUCE rule-name) | (REDUCE0 rule-name)
;;        | (POP-STATES integer)
;;        | (PUSH-STATE block-name)
;;        | (COMMENT stuff ...)

;; branch ::= (LCASE (lookahead block-name) ...)
;;          | (GCASE (non-term block-name) ...)
;;          | (SKIP-UNTIL (lookahead block-name) ...)
;;          | ACCEPT
;;          | RETURN
;;          | (JUMP block-name)

;; bindings ::= (boolean ...)
;; lookahead ::= (token ... [#f])  ; Terminating #f means eos.

;; error-handler ::= block-name | #f
;; token-count ::= integer | #f

;; block-name, rule-name ::= ident
;; token, non-term ::= ident
