#lang racket

(require "pdarisc-data.rkt"
         "symbol-append.rkt"
         (for-template racket))
(provide compile-pdarisc)

;; compile-pdarisc : PDA-RISC
;;                   [Syntax [Lambda (Token -> Symbol)]]
;;                   [Syntax [Lambda (Input-Stream -> Token)]]
;;                   [Syntax [Lambda (Input-Stream -> Input-Stream)]]
;;                   [Syntax [Lambda (Input-Stream -> Boolean)]]
;;                   ->
;;                   [Syntax [Lambda (Input-Stream -> Any)]]
(define (compile-pdarisc p token-convert get-token drop-token eos-stream?)
  #`(lambda (I)
      (define stk '())
      (define TR #f)
      (struct state (id))
      (struct nterm (id))
      #,(match p
          ((pdarisc _ _ _ insns)
           (compile-insn-seq* insns
                              token-convert
                              get-token
                              drop-token
                              eos-stream?)))))

;; compile-insn-seq* : Insn-Seq*
;;                     [Syntax [Lambda (Token -> Symbol)]]
;;                     [Syntax [Lambda (Input-Stream -> Token)]]
;;                     [Syntax [Lambda (Input-Stream -> Input-Stream)]]
;;                     [Syntax [Lambda (Input-Stream -> Boolean)]]
;;                     -> Syntax
;; produces the continuation representing this Insn-Seq*
(define (compile-insn-seq* insns
                           token-convert
                           stream-get-token
                           stream-drop-token
                           eos-stream?)
  (match insns
    ((list (assign _ reg var-rhs)
           etc ...)
     #`(let ((#,(compile-register reg)
              #,(compile-var-rhs var-rhs)))
         #,(compile-insn-seq* etc 
                              token-convert
                              stream-get-token
                              stream-drop-token
                              eos-stream?)))
    ((list (push _ pure-rhs)
           etc ...)
     #`(begin (set! stk (cons #,(compile-pure-rhs pure-rhs) stk))
              #,(compile-insn-seq* etc
                                   token-convert
                                   stream-get-token
                                   stream-drop-token
                                   eos-stream?)))
    ((list (sem-act _ name in-regs out-regs act)
           etc ...)
     #`(let-values ((#,(map compile-maybe-var out-regs) #,(compile-foreign act in-regs)))
         #,(compile-insn-seq* etc 
                              token-convert
                              stream-get-token
                              stream-drop-token
                              eos-stream?)))
    ((list (drop-token _)
           etc ...)
     #`(begin (set! TR #f) #,(compile-insn-seq* etc
                                                token-convert
                                                stream-get-token
                                                stream-drop-token
                                                eos-stream?)))
    ((list (get-token _)
           etc ...)
     #`(begin (set! TR (#,stream-get-token I))
              (set! I (#,stream-drop-token I))
              #,(compile-insn-seq* etc
                                   token-convert
                                   stream-get-token
                                   stream-drop-token
                                   eos-stream?)))
    ((list (block _ insns)
           etc ...)
     (compile-insn-seq* (append insns etc)  
                        token-convert
                        stream-get-token
                        stream-drop-token
                        eos-stream?))
    (else (compile-insn* (car insns) 
                         token-convert
                         stream-get-token
                         stream-drop-token
                         eos-stream?))))

(define (compile-labeled-definition name
                                    args
                                    body 
                                    token-convert
                                    stream-get-token
                                    stream-drop-token
                                    eos-stream?)
  #`(#,(compile-label-use name)
     (lambda #,(map compile-register args) #,(compile-insn-seq* body 
                                                           token-convert
                                                           stream-get-token
                                                           stream-drop-token
                                                           eos-stream?))))

(define (compile-label-use label)
  (datum->syntax #'a (symbol-append 'l- (label-name-id label))))

(define (compile-foreign a in-regs)
  #`((lambda #,(map (curry compile-register-no-rename a) in-regs) #,a)
     #,@(map compile-register in-regs)))

(define (compile-insn* insn*
                       token-convert
                       stream-get-token
                       stream-drop-token
                       eos-stream?)
  (match insn*
    ((label _ names stacks token-regs arg-lists bodies label-body)
     #`(letrec #,(map (lambda (name arg-list body)
                        (compile-labeled-definition name
                                                    arg-list
                                                    body 
                                                    token-convert
                                                    stream-get-token
                                                    stream-drop-token
                                                    eos-stream?))
                      names arg-lists bodies)
         #,(compile-insn-seq* label-body 
                              token-convert
                              stream-get-token
                              stream-drop-token
                              eos-stream?)))
    ((block* _ insns)
     (compile-insn-seq* insns 
                        token-convert
                        stream-get-token
                        stream-drop-token
                        eos-stream?))
    ((if-eos _ cnsq altr)
     #`(if (#,eos-stream? I)
           #,(compile-insn* cnsq 
                            token-convert
                            stream-get-token
                            stream-drop-token
                            eos-stream?)
           #,(compile-insn* altr 
                            token-convert
                            stream-get-token
                            stream-drop-token
                            eos-stream?)))
    ((state-case _ reg gaurds cnsqs)
     #`(case (state-id #,(compile-register reg))
         .
         #,(map (lambda (gaurd cnsq)
                  #`((#,(state-id gaurd)) #,(compile-insn-seq* cnsq 
                                                               token-convert
                                                               stream-get-token
                                                               stream-drop-token
                                                               eos-stream?)))
                gaurds
                cnsqs)))
    ((token-case _ gaurds cnsqs)
     #`(case (#,token-convert TR)
         .
         #,(map (lambda (gaurd cnsq)
                  #`(#,(if gaurd (list gaurd) #'else)
                     #,(compile-insn-seq* cnsq 
                                          token-convert
                                          stream-get-token
                                          stream-drop-token
                                          eos-stream?)))
                gaurds
                cnsqs)))
    ((go _ label args)
     #`(#,(compile-label-use label) #,@(map compile-pure-rhs args)))
    ((accept _ regs)
     #`(values #,@(map compile-register regs)))
    ((reject _)
     #`(error 'parser "Invalid input string."))))

(define (compile-maybe-var mv)
  (if mv (compile-register mv) #'__))
(define (compile-var-rhs rhs)
  (if (pop? rhs)
      #`(begin0 (car stk) (set! stk (cdr stk)))
      (compile-pure-rhs rhs)))
(define (compile-pure-rhs rhs)
  (match rhs
    ((register) (compile-register rhs))
    ((state id) #`(state '#,id))
    ((nterm id) #`(nterm '#,id))
    ((curr-token n) #`TR)))
(define (compile-register r)
  (match r
    ((named-reg name) (datum->syntax #'a (symbol-append 'r- name)))
    ((nameless-reg) #'__)))
;; act is there for syntax location.
(define (compile-register-no-rename act r)
  (match r
    ((named-reg name) (datum->syntax act name))
    ((nameless-reg) #'__)))
