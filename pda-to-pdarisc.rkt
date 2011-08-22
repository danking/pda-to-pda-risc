#lang racket
(require "pda-data.rkt"
         "parse-high-level-pda.rkt")
(provide produce-risc-pda)

;; produce-risc-pda : SExp
;; sexp should be a pda sexp as specified by the output of Olin's LALR
;; parser generator
(define (produce-risc-pda sexp)
  (let* ((pda (parse-pda sexp)))
    `(label
      ,(append (produce-state-blocks pda)
               (produce-rule-blocks pda))
      (go ,(pda-start pda)))))

;; produce-rule-blocks : PDA -> SExp
;; Each rule in the pda is converted to two pda-risc rule label clauses.
;; In the first clause, the pda is not known to be in the EOS state. In the
;; second clause, the pda is known to be in the EOS state.
(define (produce-rule-blocks pda)
  (let* ((rules (pda-rules pda)))
    (foldl (lambda (rule rules)
             (append (translate-rule pda rule) rules))
           '()
           rules)))

;; produce-state-blocks : PDA -> SExp
;; a glue procedure which folds `translate-state` over the states in a PDA
(define (produce-state-blocks pda)
  (let ((states (pda-states pda)))
    (foldl (lambda (state states)
             (append (translate-state state (pda-eos pda)) states))
           '()
           states)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; states

(define (translate-state st eos-token)
  (let ((name (state-name st))
        (actions (state-non-gotos st)))
   (list (risc-top-state name)
         (risc-eos-state name actions eos-token)
         (risc-have-token-state name actions eos-token))))

(define (risc-top-state name)
  `(,name ()
          (if-eos (go ,(make-eos-name name))
                  (block get-token
                         (go ,(make-have-token-name name))))))

(define (risc-eos-state name actions eos-token)
  `(,(make-eos-name name)
    ()
    .
    ,(foldr (lambda (action insns)
              (if (or (eq? (action-lookahead action) eos-token)
                      (eq? (action-lookahead action) #t))
                  (cons (risc-action-eos name action) insns)
                  insns))
            '()
            actions)))

(define (risc-have-token-state name actions eos-token)
  `(,(make-have-token-name name)
    ()
    (token-case
     ,@(foldr (lambda (action insns)
                (if (eq? (action-lookahead action) eos-token)
                    insns
                    (cons (list (action-lookahead action)
                                (risc-action name action))
                          insns)))
              '()
              actions))))

(define (risc-action from action)
  (match action
    ((shift lookahead to) (risc-shift-insn from to))
    ((accept lookahead _) risc-accept-insn)
    ((reduce lookahead to) `(go ,to))))

(define (risc-action-eos from action)
  (match action
    ((accept lookahead _) risc-accept-insn)
    ((reduce lookahead to) `(go ,(make-eos-name to)))))

(define risc-accept-insn
  '(block (:= final-semantic-value (pop))
          (accept final-semantic-value)))

(define (risc-shift-insn from to)
  `(block (push (state ,from))
          (push (current-token))
          drop-token
          (go ,to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(define (translate-rule pda r)
  (let ((name (rule-name r))
        (nt (rule-nt r))
        (n (rule-bindings r))
        (sem-act (rule-sem-act r)))
    (list (risc-rule-skeleton name
                              n
                              sem-act
                              (risc-rule-clauses nt
                                                 (pda-states pda)
                                                 make-have-token-name))
          (risc-rule-skeleton (make-eos-name name)
                              n
                              sem-act
                              (risc-rule-clauses nt
                                                 (pda-states pda)
                                                 make-eos-name)))))

(define (risc-rule-skeleton name n sem-act case-clauses)
  (define (args n)
    (build-list n (lambda (n) (SN "v" (add1 n)))))
  (define (pops n)
    (for/fold
        ([pops '()])
        ([i (in-range n 0 -1)])
      `((:= ,(SN "v" i) (pop))
        (:= ,(SN "st" i) (pop))
        .
        ,pops)))

  `(,name ()
          ,@(pops n)
          (semantic-action ,(args n) (result) ,sem-act)
          .
          ,(if (zero? n)
               `((:= v (pop))
                 (:= st (pop))
                 (push st)
                 (push v)
                 (push st)
                 (push result)
                 (state-case st . ,case-clauses))
               `((push ,(SN "st" n))
                 (push result)
                 (state-case ,(SN "st" n) . ,case-clauses)))))

(define (risc-rule-clauses nt states target-transformer)
  (define (relevant-gotos gotos)
    (filter (lambda (g) (eq? (action-lookahead g) nt)) gotos))

  (foldl (lambda (st cls)
           (let ((gotos (relevant-gotos (state-gotos st))))
             (if (empty? gotos)
                 cls
                 (cons `(,(state-name st) (go ,(target-transformer
                                                (action-target (first gotos)))))
                       cls))))
         '()
         states))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; symbol frobbing

;; Symbol Symbol -> Symbol
;; appends the string versions of the symbols to produce a new symbol
;; (symbol-append 'foo 'bar) => 'foobar
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))

(define (SN s n)
  (string->symbol (string-append s (number->string n))))

(define (make-eos-name name)
  (symbol-append name '-eos))

(define (make-have-token-name name)
  (symbol-append name '-have-token))

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
