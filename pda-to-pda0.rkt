#lang racket
(provide convert-pda)

;; The data definitions (i.e. grammars) are after the code

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
    [else     (error 'convert-pda "unsupported pda-clause : ~a"
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
  (let ((name (second state))
        (actions (group-actions (cddr state))))
    (produce-state-block name
                         (first actions)
                         (second actions)
                         accept-block)))

;; group all the gotos together, must maintain order of others per pda specs
(define (group-actions actions)
  (let loop ((gotos '())
             (others '())
             (actions actions))
    (cond [(empty? actions)
           (list (reverse gotos) (reverse others))]
          [(eq? (caar actions) 'goto)
           (loop (cons (car actions) gotos) others (rest actions))]
          [else
           (loop gotos (cons (car actions) others) (rest actions))])))

;; expands a state (name, goto actions, other actions) into its primary state
;; and its reduction state
(define (produce-state-block name gotos other-actions accept-block)
  (let ((reduce-state (string->symbol
                       (string-append (symbol->string name)
                                      "-reduce"))))
    `((block ,name #f #f
             (push-state ,reduce-state)
             (lcase . ,(format-case-clauses other-actions accept-block)))
      (block ,reduce-state #f #f
             (push-state ,reduce-state)
             (gcase . ,(format-case-clauses gotos accept-block))))))

;; trims off the clause type identifier and contorts accept clauses so that
;; they point to the accepting block
(define (format-case-clauses clauses accept-block)
  (map (lambda (x)
         (if (eq? (first x) 'accept)
             `(,(second x) ,accept-block)
             `(,(second x) ,(third x))))
       clauses))
