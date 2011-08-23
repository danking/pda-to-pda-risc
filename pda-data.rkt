#lang racket
(provide (all-defined-out))
(require (rename-in racket
                    (define-struct define-struct*)))

(define-syntax symbol-append
  (syntax-rules ()
    [(_ sym ...)
     (string->symbol (string-append (symbol->string sym) ...))]))

(define-syntax (define-ustruct stx)
  (define (make-updater-name struct-name field-name)
    (symbol-append struct-name '-update- field-name))
  (define (make-updater-proc name-stx field-stx)
    (let* ((field-name (syntax->datum field-stx))
           (struct-name (syntax->datum name-stx))
           (update-proc (datum->syntax name-stx
                                       (make-updater-name struct-name
                                                          field-name))))
      #`(define (#,update-proc val s)
          (struct-copy #,name-stx s [#,field-stx val]))))

  (syntax-case stx ()
      ((_ name fields kws ...)
       #`(begin (define-struct name fields kws ...)
                #,@(syntax-map (lambda (f)
                                 (make-updater-proc #'name f))
                               #'fields)))))


;; A PDA is a (make-pda [ListOf Symbol]
;;                      [Maybe Symbol]
;;                      [Maybe Symbol]
;;                      [ListOf State]
;;                      [ListOf Rule])
(define-ustruct pda (tokens eos start states rules))

(define empty-pda (make-pda '() #f #f '() '()))

;; a State is a (make-state Symbol
;;                          [ListOf (U Shift
;;                                     Reduce
;;                                     Accept)]
;;                          Goto)
(define-ustruct state (name non-gotos gotos))

;; a Rule is a (make-rule Symbol
;;                        Symbol
;;                        [ListOf [Maybe Symbol]]
;;                        SExp)
(define-ustruct rule (name nt bindings sem-act))


;; an Action is a (make-action Symbol)
(define-ustruct action (lookahead))
;; a TAction is a (make-taction Symbol Symbol)
(define-ustruct (taction action) (target))

(define-ustruct (shift   taction) ())
(define-ustruct (reduce  taction) ())
(define-ustruct (goto    taction) ())
(define-ustruct (accept  action) ())
