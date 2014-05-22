#lang racket
(require (rename-in racket
                    (define-struct define-struct*)))
(provide define-ustruct)

;; (define-syntax symbol-append
;;   (syntax-rules ()
;;     [(_ sym ...)
;;      (string->symbol (string-append (symbol->string sym) ...))]))

(begin-for-syntax
 (define (symbol-append . args)
   (string->symbol (apply string-append (map symbol->string args)))))

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
    ((_ (name super) fields kws ...)
     #`(begin (define-struct (name super) fields #:transparent kws ...)
              #,@(map (lambda (f)
                        (make-updater-proc #'name f))
                      (syntax->list #'fields))))
    ((_ name fields kws ...)
     #`(begin (define-struct name fields #:transparent kws ...)
              #,@(map (lambda (f)
                        (make-updater-proc #'name f))
                      (syntax->list #'fields))))))
