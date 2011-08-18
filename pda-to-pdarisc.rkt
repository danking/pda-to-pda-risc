#lang racket
(require "pda-data.rkt"
         "parse-high-level-pda.rkt")
(provide produce-risc-pda)

;; produce-risc-pda : SExp
;; sexp should be a pda sexp as specified by the output of Olin's LALR
;; parser generator
(define (produce-risc-pda sexp)
  (let* ((pda-no-hsh (parse-pda sexp))
         (pda (pda-set-reducible-states (build-state-rule-hash pda-no-hsh)
                                        pda-no-hsh)))
    `(label
      ,(append (produce-state-blocks pda)
               (produce-rule-blocks pda))
      (go ,(pda-start pda)))))

;; produce-rule-blocks : PDA -> SExp
;; Each rule in the pda is converted to two pda-risc rule label clauses.
;; In the first clause, the pda is not known to be in the EOS state. In the
;; second clause, the pda is known to be in the EOS state.
(define (produce-rule-blocks pda)
  (let* ((rules (pda-rules pda))
         (hsh (pda-reducible-states pda)))
    (foldl (lambda (rule rules)
             (append (make-risc-rules rule hsh) rules))
           '()
           rules)))

(define (make-risc-rules r hsh)
  (let* ((states (hash-ref hsh
                           (rule-name r)
                           (lambda ()
                             (error 'rule-state-case
                                    "rule has no reducible states: ~a in ~a"
                                    (rule-name r) hsh))))
         (eos-states (map make-eos-name states)))
    (match r
      ((rule name nt bindings sem-act)
       (list (rule-skeleton name
                            bindings
                            sem-act
                            (rule-case-clauses nt
                                               states
                                               states))
             (rule-skeleton (make-eos-name name)
                            bindings
                            sem-act
                            (rule-case-clauses
                             nt
                             (append states eos-states)
                             (append eos-states eos-states))))))))

(define (rule-case-clauses nt from-states to-states)
  (map (lambda (from to)
         `(,from (go ,to (nterm ,nt) result)))
       from-states
       to-states))

(define (rule-skeleton name bindings sem-action case-clauses)
  (define (vN n)
    (string->symbol (string-append "v" (number->string n))))
  (define (generate-pops n)
    (for/fold
        ([pops '()])
        ([i (in-range n 0 -1)])
      `((:= ,(vN i) (pop))
        (:= dummy (pop))
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
              (map (lambda (s)
                     (symbol-append (state-name s) '-reduce))
                   (filter (lambda (s)
                             (ormap (lambda (g)
                                      (eq? (rule-nt r)
                                           (second g)))
                                    (state-gotos s)))
                           states))))))

;; produce-state-blocks : PDA -> SExp
;; a glue procedure which folds `make-risc-states` over the states in a PDA
(define (produce-state-blocks pda)
  (let ((states (pda-states pda)))
    (foldl (lambda (state states)
             (append (make-risc-states state (pda-eos pda))
                     states))
           '()
           states)))

;; make-risc-states : PDAState Symbol ->  (list SExp SExp SExp SExp)
;; Each high-level PDA state corresponds to (at most) four PDA-RISC states;
;; therefore, `make-risc-states produces a body state, a reduce state, and
;; an -eos version of each input state
(define (make-risc-states st eos-token)
  (match st
    [(state name not-gotos gotos)
     (let* ((reduce-name (symbol-append name '-reduce))
            (eos-name (make-eos-name name))
            (eos-reduce-name (make-eos-name reduce-name)))
       (let-values
           (((eos-others non-eos-others) (segregate-eos not-gotos eos-token))
            ((actions-w/o-lookahead)     (filter (lambda (x)
                                                   (eq? (second x) #t))
                                                 not-gotos)))
         (list (make-body-state name reduce-name non-eos-others eos-others)
               (make-reduce-state reduce-name gotos)
               (make-eos-body-state eos-name
                                    eos-reduce-name
                                    (append actions-w/o-lookahead
                                            eos-others))
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
                    drop-token
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
        `(,lookahead-token (block (:= dummy (pop))
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
