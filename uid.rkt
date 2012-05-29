#lang racket
(provide init)

;; (require "uid.rkt")
;; (define-values
;;   (next-uid current-uid reset-uid! set-uid!)
;;   (init))


;; next-uid : -> Integer
;; current-uid : -> Integer
;; reset-uid! : -> Void
;; set-uid! : Integer -> Void
(define (init)
  (let ((counter 0))
    (values (lambda ()
              (set! counter (add1 counter))
              counter)
            (lambda ()
              counter)
            (lambda ()
              (set! counter 0))
            (lambda (uid)
              (set! counter uid)))))
