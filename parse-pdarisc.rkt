#lang racket
(require "pdarisc-data.rkt"
         "uid.rkt")
(provide parse-pdarisc parse-insn parse-insn* parse-insn*-seq
         parse-var-rhs parse-pure-rhs)

(define-values
  (next-uid current-uid reset-uid set-uid)
  (init))

(define (parse-pdarisc insn*-seq)
  (let ((insns (parse-insn*-seq insn*-seq)))
    (make-pdarisc (current-uid) insns)))

(define (parse-insn i)
  (define r parse-insn)
  (define (rs ls) (map r ls))

  (match i
    (`(:= ,id ,val)
     (make-assign (next-uid) (parse-reg id) (parse-var-rhs val)))
    (`(push ,val)
     (make-push (next-uid) (parse-pure-rhs val)))
    (`(semantic-action ,name
                       (,params ...)
                       (,retvars ...)
                       ,action)
     (make-sem-act (next-uid)
                   (syntaxify name)
                   (map parse-reg params)
                   (map (maybe-f parse-reg) retvars)
                   action))
    ('drop-token
     (make-drop-token (next-uid)))
    ('get-token
     (make-get-token (next-uid)))
    (`(stack-ensure ,hdrm)
     (make-stack-ensure (next-uid) hdrm))
    (`(block . ,insns)
     (make-block (next-uid) (rs insns)))))

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
    (`(label ((,ids ,stack-type ,token-type
                    (,param-list ...) ,label-body ...) ...)
             ,body ...)
     (make-label (next-uid)
                 (map parse-label-name ids)
                 stack-type
                 token-type
                 (map (lambda (plist) (map parse-reg plist)) param-list)
                 (map rs* label-body)
                 (rs* body)))
    (`(block ,insns ...)
     (make-block* (next-uid) (rs* insns)))
    (`(accept ,vars ...)
     (make-accept (next-uid) (map parse-reg vars)))
    (`(reject)
     (make-reject (next-uid)))
    (`(if-eos ,cnsq ,altr)
     (make-if-eos (next-uid) (r* cnsq) (r* altr)))
    (`(state-case ,var (,looks . ,cnsqs) ...)
     (make-state-case (next-uid)
                      (parse-reg var)
                      (map (compose parse-pure-rhs (lambda (x) `(state ,x)))
                           looks)
                      (map rs* cnsqs)))
    (`(token-case (,looks . ,cnsqs) ...)
     (make-token-case (next-uid)
                      (map (maybe-f (compose make-state syntaxify))
                           looks)
                      (map rs* cnsqs)))
    (`(go ,target ,args ...)
     (go (next-uid)
         (parse-label-name target)
         (map parse-pure-rhs args)))))

(define (parse-var-rhs r)
  (match r
    (`(pop) (make-pop))
    (_ (parse-pure-rhs r))))

(define (parse-pure-rhs r)
  (match r
    (`(state ,id)
     (make-state (syntaxify id)))
    (`(nterm ,id)
     (make-nterm id))
    ('(current-token)
     (make-curr-token #f))
    (`(current-token ,n)
     (make-curr-token n))
    ((? symbol? id)
     (parse-reg id))))

(define (parse-reg r)
  (if (eq? r '_)
      (make-nameless-reg)
      (make-named-reg r)))

(define (parse-label-name l)
  (make-label-name l))

;; [X -> X] -> [[Maybe X] -> X]
(define (maybe-f f)
  (lambda (x) (if x (f x) x)))

(define (syntaxify val)
  (quasisyntax (unsyntax val)))
