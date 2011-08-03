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
        (foldr (lambda (clause more) (convert-pda-clause clause more))
               '()
               (lower-case pda))))

;; dispatches to various pda-clause converters
(define (convert-pda-clause pda-clause more)
  (case (first pda-clause)
    [(state)   (append (convert-state pda-clause)
                       more)]
    [(rule)    (cons (convert-rule pda-clause)
                     more)]
    [(tokens no-shift error)  (cons pda-clause more)]
    [(comment) more]
    [else      (error 'convert-pda "unsupported pda-clause : ~a"
                      pda-clause)]))

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
(define (convert-state state)
  (let ((name (cadr state))
        (actions (cddr state)))
   (let-values (((goto-actions other-actions)
                 (categorize-actions actions)))
     (produce-state-block name
                          goto-actions
                          other-actions))))

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
;; and its reduction state, ferrying eos actions to the true branch of if-eos
(define (produce-state-block name gotos other-actions)
  (let* ((reduce-state (symbol-append name '-reduce))
         (uses-eos? (lambda (state) (eq? (cadr state) '$eos)))
         (maybe-eos-action (filter uses-eos? other-actions))
         (non-eos-actions (filter (lambda (x) (not (uses-eos? x)))
                                  other-actions)))
    `((,name ()
             (push (state ,reduce-state))
             (if-eos
              (block . ,(if (empty? maybe-eos-action)
                            '...
                            (cadar (format-non-goto-actions maybe-eos-action))))
              (block get-token
                     (push (current-token))
                     (token-case . ,(format-non-goto-actions
                                     non-eos-actions)))))
      (,reduce-state (nt sem-val)
                     (push (state ,reduce-state))
                     (push sem-val)
                     (state-case nt . ,(format-goto-actions gotos))))))

;; Symbol Symbol -> Symbol
;; appends the string versions of the symbols to produce a new symbol
;; (symbol-append 'foo 'bar) => 'foobar
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))

;; trims off the clause type identifier and contorts accept clauses so that
;; they point to the accepting block
(define (format-non-goto-actions clauses)
  (map (lambda (x)
         (if (eq? (first x) 'accept)
             `(,(second x) ((pop)
                            (:= return-value (pop))
                            (accept return-value)))
             `(,(second x) (go ,(third x)))))
       clauses))

;; similar to `format-non-goto-actions', but this function needn't extract the
;; lookahaed from a list
(define (format-goto-actions clauses)
  (map (lambda (x)
         `(,(second x) (go ,(third x))))
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
