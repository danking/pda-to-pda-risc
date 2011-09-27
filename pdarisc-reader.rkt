#lang racket
(require "pdarisc-data.rkt")
(provide read-pdarisc read-insn)

(define (read-pdarisc insn*-seq)
  (make-pdarisc (read-insn*-seq insn*-seq)))



(define (read-insn i)
  (define r read-insn)
  (define (rs ls) (map r ls))

  (match i
    (`(:= ,id ,val)
     (make-assign id (read-var-rhs val)))
    (`(push ,val)
     (make-push (read-pure-rhs val)))
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

(define (read-insn*-seq seq)
  (foldr (lambda (x xs)
           (if (empty? xs)
               (list (read-insn* x))
               (cons (read-insn x) xs)))
         '()
         seq))

(define (read-insn* i)
  (define r* read-insn*)
  (define rs* read-insn*-seq)

  (match i
    (`(label ((,ids (,param-list ...) ,label-body ...) ...)
             ,body ...)
     (make-label ids param-list (map rs* label-body) (rs* body)))
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
     (go target (map read-pure-rhs args)))))

(define (read-var-rhs r)
  (match r
    (`(pop) (make-pop))
    (_ (read-pure-rhs r))))

(define (read-pure-rhs r)
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