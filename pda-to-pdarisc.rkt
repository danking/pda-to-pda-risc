#lang racket
(provide convert-pda)

(define (lower-case x)
  (cond [(cons? x)
         (cons (lower-case (car x)) (lower-case (cdr x)))]
        [(symbol? x)
         (string->symbol (string-downcase (symbol->string x)))]
        [else x]))

;; convert-pda : [ListOf Clause] -> [ListOf Clause]
;; convert a pda description into a pda0 description
(define (convert-pda pda)
  (list 'label
        (foldr (lambda (clause more) (convert-pda-clause clause
                                                         more
                                                         (get-eos-token pda)))
               '()
               (lower-case pda))
        '(go **start-symbol**?)))

;; dispatches to various pda-clause converters
(define (convert-pda-clause pda-clause more the-eos-token)
  (case (first pda-clause)
    [(state)   (append (convert-state pda-clause the-eos-token)
                       more)]
    [(rule)    (cons (convert-rule pda-clause)
                     more)]
    [(tokens no-shift error) (cons pda-clause more)]
    [(comment eos) more]
    [else      (error 'convert-pda "unsupported pda-clause : ~a"
                      pda-clause)]))

;; grabs the first occurence of (eos . stuff) and returns the associated token
(define (get-eos-token pda)
  (second (assq 'eos pda)))

;; produces a rule block which handles reduction, state pop'ing, and jumping
(define (convert-rule rule)
  (define (vN n)
    (string->symbol (string-append "v" (number->string n))))
  (define (generate-pops n)
    (let loop ((i n)
               (pops '()))
      (if (zero? i)
          pops
          (loop (sub1 i)
                `((:= ,(vN i) (pop))
                  (pop)
                  .
                  ,pops)))))
  (define (generate-args bools)
    (let loop ((i 1)
               (ls bools))
      (cond [(empty? ls) empty]
            [(first ls) (cons (vN i)
                              (loop (add1 i)
                                    (rest ls)))]
            [else (loop (add1 i) (rest ls))])))

  (let ((name (second rule))
        (nterm (third rule))
        (vars (fourth rule))
        (sem-action (fifth rule)))
    `(,name ()    ; blocks and rules have different namespaces
            ,@(generate-pops (length vars))
            (semantic-action ,(generate-args vars)
                             (result)
                             ,sem-action)
            (:= return-here (pop))
            (go return-here (nterm ,nterm) result))))

;; produce a series of pda0 blocks which represent the given pda state
(define (convert-state state the-eos-token)
  (let ((name (cadr state)))
    (let*-values
        (((gotos others) (segregate-gotos (remove-comments (cddr state))))
         ((eos-actions actions) (segregate-eos others the-eos-token)))
      (make-risc-states name gotos actions eos-actions))))

(define (make-risc-states name gotos others eos-actions)
  (let ((reduce-name (symbol-append name '-reduce)))
    (list (make-body-state name reduce-name others eos-actions)
          (make-reduce-state reduce-name gotos))))

(define (make-body-state name reduce-name others eos-actions)
  `(,name ()
          (push (state ,reduce-name))
          (if-eos
           (block . ,(map (lambda (x)
                            (cadr (convert-action x)))
                          eos-actions))
           (block get-token
                  (push (current-token))
                  (token-case . ,(map convert-action others))))))

(define (make-reduce-state reduce-name gotos)
  `(,reduce-name (nt sem-val)
                 (push (state ,reduce-name))
                 (push sem-val)
                 (state-case nt . ,(map convert-goto gotos))))

(define (segregate-gotos gotos+others)
  (partition (lambda (x) (of-type? x 'goto)) gotos+others))

(define (segregate-eos actions the-eos-token)
  (partition (lambda (x) (and (cons? (second x))
                              (eq? (first (second x)) the-eos-token)))
             actions))

(define (remove-comments clauses)
  (filter (lambda (x) (not (of-type? x 'comment))) clauses))

(define (of-type? tuple type)
  (eq? (first tuple) type))

(define (convert-action action)
  (let ((lookahead-token (second action)))
    (if (of-type? action 'accept)
        `(,lookahead-token (block (pop)
                                  (:= return-value (pop))
                                  (accept return-value)))
        `(,lookahead-token (go ,(third action))))))

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
