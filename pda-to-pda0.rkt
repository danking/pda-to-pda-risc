#lang racket
;; accept -> start $end
;; start -> foo C
;;        | foo B foo
;; foo -> A
;;      | foo A

;; CFG from doc.txt
;; ((tokens A B C)
;;  (non-term start
;;            (=> (foo C) 0)
;;            (=> (foo B foo) 0))
;;  (non-term foo
;;            (=> (A) 0)
;;            (=> (foo A) 0)))

;; PDA from doc.txt
;; ((tokens A B C)
;;  (state s1
;;         (shift (A) s2)
;;         (goto start s8)
;;         (goto foo s3))
;;  (state s2
;;         (reduce () r3))
;;  (state s3
;;         (shift (A) s4)
;;         (shift (C) s5)
;;         (shift (B) s6))
;;  (state s4
;;         (reduce () r4))
;;  (state s5
;;         (reduce () r1))
;;  (state s6
;;         (shift (A) s2)
;;         (goto foo s7))
;;  (state s7
;;         (shift (A) s4)
;;         (reduce () r2))
;;  (state s8
;;         (accept ($end)))

;;  (rule r1 start  (#f #f)    1)
;;  (rule r2 start  (#f #f #f) 2)
;;  (rule r3 foo    (#f)       3)
;;  (rule r4 foo    (#f #f)    4)
;;  (rule r5 accept (#f #f)    5))

(provide convert-pda
         convert-rule
         convert-state
         group-actions)

;; convert a pda description into a pda0 description
(define (convert-pda pda)
  (let* ((used-names (map second
                          (filter state?
                                  pda)))
         (accept-block (uniqify 'accept-block used-names)))
    (foldr (lambda (pda-clause more)
             (case (first pda-clause)
               [(state) (append (convert-state pda-clause
                                                accept-block)
                                 more)]
               [(rule) (list* (convert-rule pda-clause
                                            accept-block)
                              pda-clause ; include the rule itself
                              more)]
               [(tokens) (cons pda-clause more)]
               [else (error 'convert-pda "unsupported pda-clause : ~a"
                            pda-clause)]))
           `((block ,accept-block #f #f accept))
           pda)))

;; produce an id, based on name, which is unique relative to used
(define (uniqify name used)
  (let loop ((name name)
             (used used))
    (if (memq name used)
        (loop (string->symbol
               (string-append "_" (symbol->string name)))
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
    `(block ,name #f #f         ; blocks and rules have different namespaces (?)
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
