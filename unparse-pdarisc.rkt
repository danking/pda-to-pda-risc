#lang racket
(require "pdarisc-data.rkt")
(provide unparse-pdarisc unparse-insn)

(define (unparse-pdarisc p)
  (unparse-insn-seq* (pdarisc-insns p)))



(define (unparse-insn i)
  (match i
    ((assign id val)
     `(:= ,id ,(unparse-var-rhs val)))
    ((push val)
     `(push ,(unparse-pure-rhs val)))
    ((sem-act params retvars action)
     `(semantic-action ,params
                       ,retvars
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
    `(accept . ,vars))
   ((if-eos cnsq altr)
    `(if-eos ,(unparse-insn* cnsq) ,(unparse-insn* altr)))
   ((state-case var looks cnsqs)
    `(state-case ,(unparse-var-rhs var)
                 . ,(map cons looks (map up-seq* cnsqs))))
   ((token-case looks cnsqs)
    `(token-case . ,(map cons looks (map up-seq* cnsqs))))
   ((go target args)
    `(go ,target . ,(map unparse-pure-rhs args)))))

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
   ((var-ref id)
    id)
   ((state id)
    `(state ,id))
   ((nterm id)
    `(nterm ,id))
   ((curr-token #f)
    '(current-token))
   ((curr-token n)
    `(current-token ,n))))

(define (unparse-label-clauses ids stack-types token-types param-lists rhses)
  (map (lambda (id stack-type token-type param rhs)
         `(,id : ,stack-type ,token-type ,param . ,(unparse-insn-seq* rhs)))
       ids
       stack-types
       token-types
       param-lists
       rhses))
