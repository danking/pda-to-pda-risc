#lang racket
(require "pdarisc-data.rkt"
         "symbol-append.rkt"
         (for-template racket))
(provide compile-pdarisc)

;; compile-insn-seq* : PDA-RISC
;;                     [Syntax [Lambda (Token -> Symbol)]]
;;                     [Syntax [Lambda (Input-Stream -> Token)]]
;;                     [Syntax [Lambda (Input-Stream -> Input-Stream)]]
;;                     ->
;;                     [Syntax [Lambda (Input-Stream -> Any)]]
(define (compile-pdarisc p token-convert get-token drop-token eos-stream?)
  (match p
    ((pdarisc insns)
     #`(lambda (input-stream)
         (define-struct nterm (id) #:transparent)
         (define-struct state (id) #:transparent)
         (#,(compile-insn-seq* insns
                               token-convert
                               get-token
                               drop-token
                               eos-stream?)
          input-stream
          #f
          (hasheq)
          '())))))

;; compile-pure-rhs : Pure-Rhs
;;                    [Syntax Identifier]
;;                    [Syntax Identifier]
;;                    ->
;;                    [Syntax Expression]
;; tr and regs are the corresponding symbols to the token-register and
;; register-environment identifiers in this context
(define (compile-pure-rhs r tr regs)
  (match r
    ((state id) #`(make-state '#,id))
    ((nterm id) #`(make-nterm '#,id))
    ((curr-token #f) tr)
    ((register) (compile-register-ref r regs))))

;; compile-reg : Register -> [Syntax Identifier]
(define (compile-register-name r)
  (match r
    ((named-reg id) id)
    ((nameless-reg) #'<nameless-register>)))

;; compile-reg-ref : Register [Syntax Identifier] -> Syntax
(define (compile-register-ref r regs)
  #`(dict-ref #,regs '#,(compile-register-name r)))

;; compile-insn-seq* : Insn-Seq*
;;                     [Syntax [Lambda (Token -> Symbol)]]
;;                     [Syntax [Lambda (Input-Stream -> Token)]]
;;                     [Syntax [Lambda (Input-Stream -> Input-Stream)]]
;;                     -> [Syntax [Lambda ([ListOf Token]
;;                                         Token
;;                                         [Dict Symbol Value]
;;                                         [ListOf Value]
;;                                         ->
;;                                         Any)]]
;; produces the continuation representing this Insn-Seq*
(define (compile-insn-seq* insns
                           token-convert
                           stream-get-token
                           stream-drop-token
                           eos-stream?)
  (define (c insns)
    (compile-insn-seq* insns
                       token-convert
                       stream-get-token
                       stream-drop-token
                       eos-stream?))
  (define cp compile-pure-rhs)
  (define creg compile-register-name)
  (define cref compile-register-ref)

  (match insns
    ((list (assign reg (pop))
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc)
          in
          tr
          (dict-set regs '#,(creg reg) (car stack))
          (cdr stack))))
    ((list (assign reg pure-rhs)
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc) in tr
          (dict-set regs '#,(creg reg) #,(cp pure-rhs #'tr #'regs))
          stack)))
    ((list (push pure-rhs)
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc) in tr regs (cons #,(cp pure-rhs #'tr #'regs) stack))))
    ((list (sem-act name in-regs out-regs act)
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc) in tr
          (dict-set regs '#,(creg (first out-regs))
                    ((lambda #,(map creg in-regs)
                       #,act)
                     . #,(map (lambda (x) (cref x #'regs)) in-regs)))
          stack)))
    ((list (drop-token)
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc) in #f regs stack)))
    ((list (get-token)
           etc ...)
     #`(lambda (in tr regs stack)
         (#,(c etc)
          (#,stream-drop-token in)
          (#,stream-get-token in)
          regs stack)))
    ((list (block insns)
           etc ...)
     (c (append insns etc)))
    ((list (label names stacks token-regs arg-lists bodies label-body))
     #`(lambda (in tr regs stack)
         (letrec #,(map (lambda (name arg-list body)
                          (compile-cond-clause name arg-list body c))
                        names arg-lists bodies)
           (#,(c label-body) in tr regs stack))))
    ((list (block* insns))
     (c insns))
    ((list (if-eos cnsq altr))
     #`(lambda (in tr regs stack)
         (if (#,eos-stream? in)
             (#,(c (list cnsq)) in tr regs stack)
             (#,(c (list altr)) in tr regs stack))))
    ((list (state-case reg gaurds cnsqs))
     #`(lambda (in tr regs stack)
         (case (state-id #,(cref reg #'regs))
           .
           #,(map (lambda (gaurd cnsq)
                    #`((#,(state-id gaurd))
                       (#,(c cnsq) in tr regs stack)))
                  gaurds
                  cnsqs))))
    ((list (token-case gaurds cnsqs))
     #`(lambda (in tr regs stack)
         (case (#,token-convert tr)
           .
           #,(map (lambda (gaurd cnsq)
                    #`(#,(if gaurd
                             (list gaurd)
                             #'else)
                       (#,(c cnsq) in tr regs stack)))
                  gaurds
                  cnsqs))))
    ((list (go label args))
     #`(lambda (in tr regs stack)
         ((#,(compile-label-name label) #,@(map (lambda (x)
                                                  (cref x #'regs))
                                                args))
          in tr regs stack)))
    ((list (accept regs))
     #`(lambda (in tr regs stack)
         #,(cons #'list (map (lambda (x) (cref x #'regs)) regs))))
    ((list (reject))
     #`(lambda (in tr regs stack)
         (format "no match in: ~a tr: ~a regs: ~a stack: ~a"
                 in tr regs stack)))))

(define (compile-label-name label)
  (match label
    ((label-polynym id id2) (datum->syntax id
                                           (symbol-append (syntax-e id)
                                                          '-
                                                          id2)
                                           id))
    ((label-name id) id)))

(define (compile-cond-clause name arg-registers body compile-cnsq)
  (define (make-dict-set key val regs)
    #`(dict-set #,regs '#,(compile-register-name key) #,val))

  (let ((lambda-args (map (compose gensym compile-register-name)
                          arg-registers)))
    #`(#,(compile-label-name name) (lambda #,lambda-args
                                     (lambda (in tr regs stack)
                                       (#,(compile-cnsq body)
                                        in
                                        tr
                                        #,(foldl make-dict-set
                                                 #'regs
                                                 arg-registers
                                                 lambda-args)
                                        stack))))))
