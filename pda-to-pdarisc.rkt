#lang racket
(provide convert-pda)

;; convert-pda : [ListOf Clause] -> [ListOf Clause]
;; convert a pda description into a pda0 description
(define (convert-pda pda)
  (list 'label
        (foldr (lambda (clause more)
                 (convert-pda-clause clause
                                     more
                                     (get-eos-token pda)))
               '()
               pda)
        `(go ,(get-start-state pda))))

;; dispatches to various pda-clause converters
(define (convert-pda-clause pda-clause more the-eos-token)
  (case (first pda-clause)
    [(state)   (append (convert-state pda-clause the-eos-token)
                       more)]
    [(rule)    (let ((rule (convert-rule pda-clause)))
                 (list* rule
                        (cons (make-eos-name (first rule))
                              (rest rule))
                        more))]
    [(tokens no-shift error) (cons pda-clause more)]
    [(comment eos start) more]
    [else      (error 'convert-pda "unsupported pda-clause : ~a"
                      pda-clause)]))

;; grabs the first occurence of (eos . stuff) and returns the associated token
(define (get-eos-token pda)
  (let ((eos-form? (assq 'eos pda)))
    (if eos-form?
        (second eos-form?)
        (error 'convert-pda "no eos token specified"))))

;; grabs the first occurence of (start . stuff) and returns the associated
;; state
(define (get-start-state pda)
  (let ((start-form? (assq 'start pda)))
    (if start-form?
        (second start-form?)
        (error 'convert-pda "no start state specified"))))

;; produces a rule block which handles reduction, state pop'ing, and jumping
(define (convert-rule rule)
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
  (define (generate-args bools)
    (for/list ([i (in-naturals 1)]
               [l bools]
               #:when l)
              (vN i)))

  (match rule
    [(list _ name nterms bindings sem-action)
     `(,name ()
             ,@(generate-pops (length bindings))
             (semantic-action ,(generate-args bindings)
                              (result)
                              ,sem-action)
            (:= return-here (pop))
            (go return-here (nterm ,nterm) result))]))

;; produce a series of pda0 blocks which represent the given pda state
(define (convert-state state the-eos-token)
  (match state
    [(list state name clauses ...)
     (let*-values
         (((gotos others) (segregate-gotos (remove-comments clauses)))
          ((eos-actions actions) (segregate-eos others the-eos-token)))
       (make-risc-states name gotos actions eos-actions))]))

;; each PDA state corresponds to (at most) four PDA-RISC states
;; we need a body state and a reduce state and an -eos version of each
(define (make-risc-states name gotos others eos-actions)
  (let* ((reduce-name (symbol-append name '-reduce))
         (eos-name (make-eos-name name))
         (eos-reduce-name (make-eos-name reduce-name)))
    (list (make-body-state name reduce-name others eos-actions)
          (make-reduce-state reduce-name gotos)
          (make-eos-body-state eos-name eos-reduce-name eos-actions)
          (make-eos-reduce-state eos-reduce-name gotos))))

(define (make-body-skeleton-state name reduce-name actions)
  `(,name ()
          (push (state ,reduce-name))
          .
          ,actions))

(define (make-eos-body-state name reduce-name eos-actions)
  (make-body-skeleton-state name
                            reduce-name
                            (map (lambda (x)
                                   (cadr (convert-eos-action x)))
                                 eos-actions)))

(define (make-body-state name reduce-name others eos-actions)
  (make-body-skeleton-state
   name
   reduce-name
   `((if-eos (block . ,(map (lambda (x)
                              (cadr (convert-eos-action x)))
                            eos-actions))
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

;; segregate the gotos from the other PDA clauses
(define (segregate-gotos gotos+others)
  (partition (lambda (x) (of-type? x 'goto)) gotos+others))

;; segregate PDA clauses whose lookahead is the given token
(define (segregate-eos actions the-eos-token)
  (partition (lambda (x) (and (cons? (second x))
                              (eq? (first (second x)) the-eos-token)))
             actions))

(define (remove-comments clauses)
  (filter (lambda (x) (not (of-type? x 'comment))) clauses))

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
    (if (of-type? action 'accept)
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
