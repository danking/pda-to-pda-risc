#lang racket
(require "pdarisc-data.rkt")
(provide read-pdarisc read-insn)

(define (read-pdarisc sexp)
  (make-pdarisc (map read-insn sexp)))



(define (read-insn i)
  (define r read-insn)
  (define (rs ls) (map r ls))

  (match i
    (`(:= ,id ,val)
     (make-assign id (read-rhs val)))
    (`(push ,val)
     (make-push (read-rhs val)))
    (`(semantic-action (,params ...)
                       (,retvars ...)
                       ,action)
     (make-sem-act params retvars action))
    ('drop-token
     (make-drop-token))
    ('get-token
     (make-get-token))
    (`(stack-ensure ,hdrm)
     (make-stack-ensure hdrm))
    (`(block . ,insns)
     (make-block (rs insns)))
    ((or (? symbol? _)
         (list (or 'state
                   'nterm
                   'current-token
                   'pop) _ ...))
     (read-rhs i))
    ((or (list 'label _ ...)
         (list 'block _ ...)
         (list 'accept _ ...)
         (list 'if-eos _ ...)
         (list 'state-case _ ...)
         (list 'token-case _ ...)
         (list 'go _ ...))
     (read-insn* i))
    ((list insns ...)
     (rs insns))))

(define (read-insn* i)
  (define r* read-insn*)
  (define (rs* ls)
    (foldr (lambda (x xs)
             (if (empty? xs)
                 (list (read-insn* x))
                 (cons (read-insn x) xs)))
           '()
           ls))

  (match i
    (`(label ((,ids (,param-list ...) ,label-body ...) ...)
             ,body ...)
     (make-label ids param-list (rs* label-body) (rs* body)))
    (`(block ,insns ...)
     (make-block* (rs* insns)))
    (`(accept ,vals ...)
     (make-accept vals))
    (`(if-eos ,cnsq ,altr)
     (make-if-eos (r* cnsq) (r* altr)))
    (`(state-case ,st (,looks . ,cnsqs) ...)
     (make-state-case st looks (rs* cnsqs)))
    (`(token-case (,looks . ,cnsqs) ...)
     (make-token-case looks (rs* cnsqs)))
    (`(go ,target ,args ...)
     (go target (map read-rhs args)))
    ((list insns ...) ;; this case is only reached by direct calls to read-insn*
     (rs* insns))))

(define (read-rhs r)
  (match r
    (`(pop)
     (make-pop))
    (`(state ,id)
     (make-state id))
    (`(nterm ,id)
     (make-nterm id))
    ('(current-token)
     (curr-token #f))
    (`(current-token ,n)
     (curr-token n))
    ((? symbol? id)
     (var-ref id))))