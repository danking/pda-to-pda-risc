#lang racket
(require "pdarisc-data.rkt")
(provide parse-pdarisc parse-insn)

(define (parse-pdarisc insn*-seq)
  (make-pdarisc (parse-insn*-seq insn*-seq)))



(define (parse-insn i)
  (define r parse-insn)
  (define (rs ls) (map r ls))

  (match i
    (`(:= ,id ,val)
     (make-assign id (parse-var-rhs val)))
    (`(push ,val)
     (make-push (parse-pure-rhs val)))
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
     (make-block (rs insns)))))

(define (parse-insn*-seq seq)
  (foldr (lambda (x xs)
           (if (empty? xs)
               (list (parse-insn* x))
               (cons (parse-insn x) xs)))
         '()
         seq))

(define (parse-insn* i)
  (define r* parse-insn*)
  (define rs* parse-insn*-seq)

  (match i
    (`(label ((,ids : ,stack-type ,token-type
                    (,param-list ...) ,label-body ...) ...)
             ,body ...)
     (make-label ids
                 stack-type
                 token-type
                 param-list
                 (map rs* label-body)
                 (rs* body)))
    (`(block ,insns ...)
     (make-block* (rs* insns)))
    (`(accept ,vals ...)
     (make-accept vals))
    (`(if-eos ,cnsq ,altr)
     (make-if-eos (r* cnsq) (r* altr)))
    (`(state-case ,st (,looks . ,cnsqs) ...)
     (make-state-case st looks (map rs* cnsqs)))
    (`(token-case (,looks . ,cnsqs) ...)
     (make-token-case looks (map rs* cnsqs)))
    (`(go ,target ,args ...)
     (go target (map parse-pure-rhs args)))))

(define (parse-var-rhs r)
  (match r
    (`(pop) (make-pop))
    (_ (parse-pure-rhs r))))

(define (parse-pure-rhs r)
  (match r
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