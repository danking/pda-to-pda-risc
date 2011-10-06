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
     (make-assign (make-named-reg id) (parse-var-rhs val)))
    (`(push ,val)
     (make-push (parse-pure-rhs val)))
    (`(semantic-action ,name
                       (,params ...)
                       (,retvars ...)
                       ,action)
     (make-sem-act name
                   (map make-named-reg params)
                   (map (lambda (x) (if x (make-named-reg x) x)) retvars)
                   action))
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
     (make-label (map make-label-name ids)
                 stack-type
                 token-type
                 (map (lambda (plist) (map make-named-reg plist)) param-list)
                 (map rs* label-body)
                 (rs* body)))
    (`(block ,insns ...)
     (make-block* (rs* insns)))
    (`(accept ,vars ...)
     (make-accept (map make-named-reg vars)))
    (`(reject)
     (make-reject))
    (`(if-eos ,cnsq ,altr)
     (make-if-eos (r* cnsq) (r* altr)))
    (`(state-case ,var (,looks . ,cnsqs) ...)
     (make-state-case (make-named-reg var)
                      (map make-state looks)
                      (map rs* cnsqs)))
    (`(token-case (,looks . ,cnsqs) ...)
     (make-token-case looks (map rs* cnsqs)))
    (`(go ,target ,args ...)
     (go (make-label-name target) (map parse-pure-rhs args)))))

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
     (make-curr-token #f))
    (`(current-token ,n)
     (make-curr-token n))
    ((? symbol? id)
     (make-named-reg id))))