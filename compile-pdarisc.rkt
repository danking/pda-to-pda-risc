#lang racket
(require "pdarisc-data.rkt")
(provide compile-pdarisc)

(define (compile-pdarisc p token-convert get-token drop-token)
  (match p
    ((pdarisc insns)
     `(lambda (input-stream)
        (define-struct nterm (id))
        (define-struct state (id))
        (,(compile-insn-seq* insns
                             token-convert
                             get-token
                             drop-token)
         input-stream
         #f
         (hasheq)
         '())))))

;; compile-pure-rhs : Pure-Rhs Symbol Symbol -> SExp
;; tr and regs are the corresponding symbols to the token-register and
;; register-environment identifiers in this context
(define (compile-pure-rhs r tr regs)
  (match r
    ((state id) `(make-state ',id))
    ((nterm id) `(make-nterm ',id))
    ((curr-token #f) tr)
    ((register) (compile-register-ref r regs))))

;; compile-reg : Register -> Symbol
(define (compile-register-name r)
  (match r
    ((named-reg id) id)
    ((nameless-reg) '<nameless-register>)))

;; compile-reg-ref : Register Symbol -> SExp
(define (compile-register-ref r regs)
  `(dict-ref ,regs ',(compile-register-name r)))

;; compile-insn-seq* : Insn-Seq*
;;                     (Token -> Symbol)
;;                     (Input-Stream -> Token)
;;                     (Input-Stream -> Input-Stream)
;;                     -> SExp
;; produces the continuation representing this Insn-Seq*. The continuation
;; is a lambda of type: Token* Token (Symbol x Value) Value* -> Value
(define (compile-insn-seq* insns
                           token-convert stream-get-token stream-drop-token)
  (define (c insns)
    (compile-insn-seq* insns token-convert stream-get-token stream-drop-token))
  (define cp compile-pure-rhs)
  (define creg compile-register-name)
  (define cref compile-register-ref)

  (match insns
    ((list (assign reg (pop))
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) in tr (dict-set regs ',(creg reg) (car stack)) (cdr stack))))
    ((list (assign reg pure-rhs)
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) in tr
                  (dict-set regs ',(creg reg) ,(cp pure-rhs 'tr 'regs))
                  stack)))
    ((list (push pure-rhs)
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) in tr regs (cons ,(cp pure-rhs 'tr 'regs) stack))))
    ((list (sem-act name in-regs out-regs act)
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) in tr
         (dict-set regs ',(creg (first out-regs))
                   ((lambda ,(map creg in-regs)
                      ,act)
                    . ,(map (lambda (x) (cref x 'regs)) in-regs)))
         stack)))
    ((list (drop-token)
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) in #f regs stack)))
    ((list (get-token)
           etc ...)
     `(lambda (in tr regs stack)
        (,(c etc) (,stream-drop-token in) (,stream-get-token in) regs stack)))
    ((list (block insns)
           etc ...)
     (c (append insns etc)))
    ((list (label names stacks token-regs arg-lists bodies label-body))
     `(lambda (in tr regs stack)
        (letrec ,(map (lambda (name arg-list body)
                        (compile-cond-clause name arg-list body c))
                      names arg-lists bodies)
          (,(c label-body) in tr regs stack))))
    ((list (block* insns))
     (c insns))
    ((list (if-eos cnsq altr))
     `(lambda (in tr regs stack)
        (if (empty? in)
            (,(c (list cnsq)) in tr regs stack)
            (,(c (list altr)) in tr regs stack))))
    ((list (state-case reg gaurds cnsqs))
     `(lambda (in tr regs stack)
        (case (state-id ,(cref reg 'regs))
          .
          ,(map (lambda (gaurd cnsq)
                  `((,(state-id gaurd))
                    (,(c cnsq) in tr regs stack)))
                gaurds
                cnsqs))))
    ((list (token-case gaurds cnsqs))
     `(lambda (in tr regs stack)
        (case (,token-convert tr)
          .
          ,(map (lambda (gaurd cnsq)
                  `(,(if gaurd
                         (list gaurd)
                         'else)
                    (,(c cnsq) in tr regs stack)))
                gaurds
                cnsqs))))
    ((list (go label args))
     `(lambda (in tr regs stack)
        ((,(compile-label-name label) ,@(map (lambda (x) (cref x 'regs)) args))
         in tr regs stack)))
    ((list (accept regs))
     `(lambda (in tr regs stack)
        ,(cons 'list (map (lambda (x) (cref x 'regs)) regs))))
    ((list (reject))
     `(lambda (in tr regs stack)
        (format "no match in: ~a tr: ~a regs: ~a stack: ~a"
                in tr regs stack)))))

(define (compile-label-name label)
  (match label
    ((label-polynym id id2) (symbol-append id '- id2))
    ((label-name id) id)))

(define (compile-cond-clause name arg-registers body compile-cnsq)
  (define (make-dict-set key val regs)
    `(dict-set ,regs ',(compile-register-name key) ,val))

  (let ((lambda-args (map (compose gensym compile-register-name)
                          arg-registers)))
    `(,(compile-label-name name) (lambda ,lambda-args
                                   (lambda (in tr regs stack)
                                     (,(compile-cnsq body)
                                      in
                                      tr
                                      ,(foldl make-dict-set
                                              'regs
                                              arg-registers
                                              lambda-args)
                                      stack))))))



(define-syntax symbol-append
  (syntax-rules ()
    [(_ sym ...)
     (string->symbol (string-append (symbol->string sym) ...))]))