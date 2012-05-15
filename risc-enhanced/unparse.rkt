#lang racket
(require "data.rkt"
         "traverse.rkt")
(provide (all-defined-out))

(define (unparse-rhs r)
  (match r
    ((pop) `(pop))
    ((state id)
     `(state ,id))
    ((nterm id)
     `(nterm ,id))
    ((curr-token #f)
     '(current-token))
    ((curr-token n)
     `(current-token ,n))
    (_ r)))

(define (unparse-insn i)
  (match i
    ((assign id val)
     `(:= ,id ,val))
    ((push val)
     `(push ,val))
    ((sem-act name params retvars action)
     `(semantic-action ,name
                       ,params
                       ,retvars
                       ,action))
    ((drop-token)
     'drop-token)
    ((get-token)
     'get-token)
    ((stack-ensure hdrm)
     `(stack-ensure ,hdrm))
    ((join-point label args)
     `(join-point ,label . ,args))
    ((block insns)
     `(block . ,insns))))

(define (unparse-insn* i)
  (match i
   ((label ids stack-types token-types param-lists rhses body)
    `(label ,(unparse-label-clauses ids stack-types token-types param-lists rhses)
            . ,body))
   ((block* insns)
    `(block . ,insns))
   ((accept vars)
    `(accept . ,vars))
   ((reject)
    `(reject))
   ((if-eos cnsq altr)
    `(if-eos ,cnsq ,altr))
   ((state-case var looks cnsqs)
    `(state-case ,var
                 . ,(map (lambda (look cnsq)
                           (cons look cnsq))
                         looks
                         cnsqs)))
   ((token-case looks cnsqs)
    `(token-case . ,(map (lambda (l c)
                           (cons (if l l #f) c))
                         looks
                         cnsqs)))
   ((go target args)
    `(go ,target . ,args))))

(define (unparse-label-clauses ids stack-types token-types param-lists rhses)
  (map (lambda (id stack-type token-type param rhs)
         `(,id : ,stack-type
               ,token-type
               ,param
               . ,rhs))
       ids
       stack-types
       token-types
       param-lists
       rhses))

(define (remove-struct:-prefix sym)
  (string->symbol (substring (symbol->string sym) 7)))

(define-values
  (unparse unparse-term unparse-term*)
  (traverse-pdarisc #:pdarisc (match-lambda ((pdarisc seq) seq))
                    #:term pda-term-insn
                    #:term* pda-term-insn
                    #:insn unparse-insn
                    #:insn* unparse-insn*
                    #:rhs unparse-rhs
                    #:syntax syntax-e
                    #:lbl (lambda (l)
                            `(label-name ,(label-name-uid l)
                                         ,(label-name-lexical-name l)))
                    #:reg (lambda (r)
                            `(register ,(register-uid r)
                                       ,(register-lexical-name r)))))


