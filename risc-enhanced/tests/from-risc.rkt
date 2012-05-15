#lang racket
(require "../../parse-pdarisc.rkt"
         "../data.rkt"
         rackunit
         "../from-risc.rkt")

(define parse-and-convert (compose convert/pdarisc parse-pdarisc))

(check-equal? (parse-and-convert '((:= foo (pop))
                                   (:= one two)
                                   (:= 我 (state state-1))
                                   (:= eat (nterm program))
                                   (:= pie (current-token))
                                   (:= _ (pop))
                                   (accept)))
              (make-pdarisc
               (list (uninitialized-pda-term (make-assign (uninitialized-register 'foo)
                                                          (make-pop)))
                     (uninitialized-pda-term (make-assign (uninitialized-register 'one)
                                                          (uninitialized-register 'two)))
                     (uninitialized-pda-term (make-assign (uninitialized-register '我)
                                                          (make-state 'state-1)))
                     (uninitialized-pda-term (make-assign (uninitialized-register 'eat)
                                                          (make-nterm 'program)))
                     (uninitialized-pda-term (make-assign (uninitialized-register 'pie)
                                                          (make-curr-token #f)))
                     (uninitialized-pda-term (make-assign (uninitialized-register '_)
                                                          (make-pop)))
                     (uninitialized-pda-term (make-accept '())))))

(check-equal? (parse-and-convert
               '((label ((hiphoppop : (id tok foo) #f
                                    (foo bar)
                                    (:= hiphop (pop))
                                    (push foo)
                                    (push bar)
                                    (accept))
                         (indirection : () plus-sign
                                      ()
                                      (go hiphoppop
                                          (nterm kanye)
                                          (nterm jay-z))))
                   (go indirection))))
              (make-pdarisc
               (list (uninitialized-pda-term
                      (make-label
                       (list (uninitialized-label-name 'hiphoppop)
                             (uninitialized-label-name 'indirection))
                       '((id tok foo) ())
                       '(#f plus-sign)
                       `((,(uninitialized-register 'foo)
                          ,(uninitialized-register 'bar))
                         ())
                       (list
                        (list
                         (uninitialized-pda-term (join-point (uninitialized-label-name 'hiphoppop)
                                                             `(,(uninitialized-register 'foo)
                                                               ,(uninitialized-register 'bar))))
                         (uninitialized-pda-term (make-assign (uninitialized-register 'hiphop)
                                                              (make-pop)))
                         (uninitialized-pda-term (make-push (uninitialized-register 'foo)))
                         (uninitialized-pda-term (make-push (uninitialized-register 'bar)))
                         (uninitialized-pda-term (make-accept '())))
                        (list
                         (uninitialized-pda-term (join-point (uninitialized-label-name 'indirection)
                                                             '()))
                         (uninitialized-pda-term (make-go
                                                  (uninitialized-label-name 'hiphoppop)
                                                  (list
                                                   (make-nterm 'kanye)
                                                   (make-nterm 'jay-z))))))
                       (list (uninitialized-pda-term (make-go
                                                      (uninitialized-label-name 'indirection)
                                                      (list)))))))))



;; (list (make-label
;;        (list (make-label-name #'hiphoppop)
;;              (make-label-name #'indirection))
;;        '((id tok foo) ())
;;        '(#f plus-sign)
;;        `((,(make-named-reg 'foo)
;;           ,(make-named-reg 'bar))
;;          ())
;;        (list
;;         (list
;;          (make-assign (make-named-reg 'hiphop)
;;                       (make-pop))
;;          (make-push (make-named-reg 'foo))
;;          (make-push (make-named-reg 'bar))
;;          (make-accept '()))
;;         (list
;;          (make-go
;;           (make-label-name #'hiphoppop)
;;           (list
;;            (make-nterm 'kanye)
;;            (make-nterm 'jay-z)))))
;;        (list (make-go
;;               (make-label-name #'indirection)
;;               (list)))))
