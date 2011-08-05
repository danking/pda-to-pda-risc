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
    [(rule)    (cons (convert-rule pda-clause)
                     more)]
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
    `(,name ()
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
                                   (cadr (convert-action x)))
                                 eos-actions)))

(define (make-body-state name reduce-name others eos-actions)
  (make-body-skeleton-state
   name
   reduce-name
   `((if-eos (block . ,(map (lambda (x)
                              (cadr (convert-action x)))
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

(define (make-eos-name name)
  (symbol-append name '-eos))

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
