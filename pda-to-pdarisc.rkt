#lang racket
(require "pda-data.rkt")
(provide produce-risc-pda)

(define (parse-pda sexp)
  (foldl (lambda (clause pda)
           (match clause
             [(list (or 'STATE 'state) name shifts-etc ...)
              (let-values
                  (((gotos not-gotos)
                    (partition (lambda (x)
                                 (eq? (car x) 'goto))
                               shifts-etc)))
                (pda-add-state (make-state name not-gotos gotos)
                               pda))]
             [(list (or 'RULE 'rule) name nt bindings sem-act)
              (pda-add-rule (make-rule name nt bindings sem-act) pda)]
             [(list (or 'EOS 'eos) token)
              (pda-set-eos token pda)]
             [(list (or 'START 'start) token)
              (pda-set-start token pda)]
             [(list (or 'TOKENS 'tokens) tokens ...)
              (pda-set-tokens tokens pda)]
             [(list (or 'COMMENT 'comment) _ ...)      pda]
             [else (begin (printf "ignoring unknown pda clause ~a\n" clause)
                          pda)]))
         empty-pda
         sexp))

(define (produce-risc-pda sexp)
  (let* ((pda-no-hsh (parse-pda sexp))
         (pda (pda-set-reducible-states (build-state-rule-hash pda-no-hsh)
                                        pda-no-hsh)))
    `(label
      ,(append (produce-state-blocks pda)
               (produce-rule-blocks pda))
      (go ,(pda-start pda)))))

(define (produce-rule-blocks pda)
  (let* ((rules (pda-rules pda))
         (hsh (pda-reducible-states pda)))
    (foldl (lambda (rule rules)
             (list* (rule-skeleton (rule-name rule)
                                   (rule-bindings rule)
                                   (rule-sem-act rule)
                                   (rule-case-clauses rule
                                                      hsh
                                                      (lambda (x) x)))
                    (rule-skeleton (make-eos-name (rule-name rule))
                                   (rule-bindings rule)
                                   (rule-sem-act rule)
                                   (rule-case-clauses rule
                                                      hsh
                                                      make-eos-name))
                    rules))
           '()
           rules)))

(define (rule-case-clauses rule reducible-states name-transformer)
  (let ((states (hash-ref reducible-states
                          (rule-name rule)
                          (lambda ()
                            (error 'rule-state-case
                                   "rule has no reducible states"))))
        (nt (rule-nt rule)))
    (map (lambda (s)
           `(,s (go ,(name-transformer s) (nterm ,nt) result)))
         states)))


(define (rule-skeleton name bindings sem-action case-clauses)
  (define (vN n)
    (string->symbol (string-append "v" (number->string n))))
  (define (generate-pops n)
    (for/fold
        ([pops '()])
        ([i (in-range n 0 -1)])
      `((:= ,(vN i) (pop))
        (pop)
        .
        ,pops)))
  (define (generate-args n)
    (build-list n (lambda (x) (vN (add1 x)))))

  `(,name ()
          ,@(generate-pops bindings)
          (semantic-action ,(generate-args bindings)
                           (result)
                           ,sem-action)
          (:= reduce-to (pop))
          (state-case reduce-to . ,case-clauses)))

;; build a hash table from rules to states which the rules might reduce to
(define (build-state-rule-hash pda)
  (let ((states (pda-states pda))
        (rules (pda-rules pda)))
    (for/hasheq
        ([r (in-list rules)])
      (values (rule-name r)
              (map state-name
                   (filter (lambda (s)
                             (ormap (lambda (g)
                                      (eq? (rule-nt r)
                                           (second g)))
                                    (state-gotos s)))
                           states))))))

(define (produce-state-blocks pda)
  (let ((states (pda-states pda)))
    (foldl (lambda (state states)
             (append (make-risc-states state (pda-eos pda))
                     states))
           '()
           states)))

;; each PDA state corresponds to (at most) four PDA-RISC states
;; we need a body statem, a reduce state, and an -eos version of each
(define (make-risc-states st eos-token)
  (match st
    [(state name not-gotos gotos)
     (let* ((reduce-name (symbol-append name '-reduce))
            (eos-name (make-eos-name name))
            (eos-reduce-name (make-eos-name reduce-name)))
       (let-values
           (((eos-others non-eos-others) (segregate-eos not-gotos eos-token)))
         (list (make-body-state name reduce-name non-eos-others eos-others)
               (make-reduce-state reduce-name gotos)
               (make-eos-body-state eos-name eos-reduce-name eos-others)
               (make-eos-reduce-state eos-reduce-name gotos))))]))

(define (make-body-skeleton-state name reduce-name actions)
  `(,name ()
          (push (state ,reduce-name))
          .
          ,actions))

(define (make-eos-body-state name reduce-name eos-others)
  (make-body-skeleton-state name
                            reduce-name
                            (map (lambda (x)
                                   (cadr (convert-eos-action x)))
                                 eos-others)))

(define (make-body-state name reduce-name others eos-others)
  (make-body-skeleton-state
   name
   reduce-name
   `((if-eos (block . ,(map (lambda (x)
                              (cadr (convert-eos-action x)))
                            eos-others))
             (block get-token
                    (push (current-token))
                    (token-case . ,(map convert-action others)))))))

(define (make-eos-reduce-state reduce-name gotos)
  (make-reduce-state reduce-name
                     (map (lambda (x)
                            (list (first x)
                                  (second x)
                                  (make-eos-name (third x))))
                          gotos)))

(define (make-reduce-state reduce-name gotos)
  `(,reduce-name (nt sem-val)
                 (push (state ,reduce-name))
                 (push sem-val)
                 (state-case nt . ,(map convert-goto gotos))))

(define (make-eos-name name)
  (symbol-append name '-eos))

;; segregate PDA clauses whose lookahead is the given token
(define (segregate-eos actions the-eos-token)
  (partition (lambda (x)
               (eq? (second x) the-eos-token))
             actions))

;; A clause is (TYPE stuff ...), so just check the head of the tuple
(define (of-type? tuple type)
  (eq? (first tuple) type))

(define (convert-action action)
  (convert-generic-action action (lambda (x) x)))

(define (convert-eos-action action)
  (convert-generic-action action make-eos-name))

;; converts a PDA action clause into a PDA-RISC token/state-case clause
(define (convert-generic-action action state-transformer)
  (let ((lookahead-token (second action)))
    (if (or (of-type? action 'accept) (of-type? action 'ACCEPT))
        `(,lookahead-token (block (pop)
                                  (:= return-value (pop))
                                  (accept return-value)))
        `(,lookahead-token (go ,(state-transformer (third action)))))))

;; converts a PDA goto clause into a PDA-RISC token/state-case clause
(define (convert-goto goto)
  (let ((lookahead-state (second goto))
        (target-state (third goto)))
    `(,lookahead-state (go ,target-state))))

;; Symbol Symbol -> Symbol
;; appends the string versions of the symbols to produce a new symbol
;; (symbol-append 'foo 'bar) => 'foobar
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))

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
