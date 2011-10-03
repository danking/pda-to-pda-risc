#lang racket
(require "pdarisc-data.rkt")
(provide unparse-pdarisc unparse-insn)

(define (unparse-pdarisc p)
  (unparse-insn-seq* (pdarisc-insns p)))



(define (unparse-insn i)
  (match i
    ((assign id val)
     `(:= ,(unparse-register id) ,(unparse-var-rhs val)))
    ((push val)
     `(push ,(unparse-pure-rhs val)))
    ((sem-act params retvars action)
     `(semantic-action ,(map unparse-register params)
                       ,(map unparse-maybe-register retvars)
                       ,action))
    ((drop-token)
     'drop-token)
    ((get-token)
     'get-token)
    ((stack-ensure hdrm)
     `(stack-ensure ,hdrm))
    ((block insns)
     `(block . ,(map unparse-insn insns)))))

(define (unparse-insn* i)
  (define up-seq* unparse-insn-seq*)

  (match i
   ((label ids stack-types token-types param-lists rhses body)
    `(label ,(unparse-label-clauses ids stack-types token-types param-lists rhses)
            . ,(up-seq* body)))
   ((block* insns)
    `(block . ,(up-seq* insns)))
   ((accept vars)
    `(accept . ,(map unparse-register vars)))
   ((if-eos cnsq altr)
    `(if-eos ,(unparse-insn* cnsq) ,(unparse-insn* altr)))
   ((state-case var looks cnsqs)
    `(state-case ,(unparse-register var)
                 . ,(map (lambda (look cnsq)
                           (cons (strip-state look)
                                 (up-seq* cnsq)))
                         looks
                         cnsqs)))
   ((token-case looks cnsqs)
    `(token-case . ,(map cons looks (map up-seq* cnsqs))))
   ((go target args)
    `(go ,(unparse-label-name target) . ,(map unparse-pure-rhs args)))))

(define (unparse-insn-seq* iseq)
  (foldr (lambda (x xs)
           (if (empty? xs)
               (list (unparse-insn* x))
               (cons (unparse-insn x) xs)))
         '()
         iseq))

(define (unparse-var-rhs r)
  (match r
    ((pop) `(pop))
    (_ (unparse-pure-rhs r))))

(define (unparse-pure-rhs r)
  (match r
   ((register)
    (unparse-register r))
   ((state id)
    (unparse-state r))
   ((nterm id)
    `(nterm ,id))
   ((curr-token #f)
    '(current-token))
   ((curr-token n)
    `(current-token ,n))))

(define (unparse-register r)
  (match r
    ((named-reg id) id)
    ((nameless-reg) '<nameless-register>)))

(define (unparse-maybe-register r)
  (match r
    (#f #f)
    (_ (unparse-register r))))

(define (unparse-state s)
  (match s
    ((state id) `(state ,id))))

(define (strip-state s)
  (match s
    ((state id) id)))

(define (unparse-label-name l)
  (match l
    ((label-polynym id extra-id)
     (symbol-append id '- extra-id))
    ((label-name id) id)))

(define (unparse-label-clauses ids stack-types token-types param-lists rhses)
  (map (lambda (id stack-type token-type param rhs)
         `(,(unparse-label-name id) : ,stack-type ,token-type
                                      ,(map unparse-register param)
                                      . ,(unparse-insn-seq* rhs)))
       ids
       stack-types
       token-types
       param-lists
       rhses))

(define-syntax symbol-append
  (syntax-rules ()
    [(_ sym ...)
     (string->symbol (string-append (symbol->string sym) ...))]))
