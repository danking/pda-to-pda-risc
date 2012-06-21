#lang racket
(require "../../parse-pdarisc.rkt"
         "../data.rkt"
         rackunit
         "../from-risc.rkt")

(define parse-and-convert (compose convert/pdarisc parse-pdarisc))

(define-test-suite
  from-risc-tests
  (test-case
   "insn*-seq"
   (check-equal? (parse-and-convert '((:= foo (pop))
                                      (:= one two)
                                      (:= 我 (state state-1))
                                      (:= eat (nterm program))
                                      (:= pie (current-token))
                                      (:= _ (pop))
                                      (accept)))
                 (make-pdarisc
                  7
                  (list (uninitialized-pda-term (make-assign 1
                                                             (uninitialized-register 'foo)
                                                             (make-pop)))
                        (uninitialized-pda-term (make-assign 2
                                                             (uninitialized-register 'one)
                                                             (uninitialized-register 'two)))
                        (uninitialized-pda-term (make-assign 3
                                                             (uninitialized-register '我)
                                                             (make-state #'state-1)))
                        (uninitialized-pda-term (make-assign 4
                                                             (uninitialized-register 'eat)
                                                             (make-nterm 'program)))
                        (uninitialized-pda-term (make-assign 5
                                                             (uninitialized-register 'pie)
                                                             (make-curr-token #f)))
                        (uninitialized-pda-term (make-assign 6
                                                             (uninitialized-register '_)
                                                             (make-pop)))
                        (uninitialized-pda-term (make-accept 7
                                                             '()))))))
  (test-case
   "label"
   (check-equal? (parse-and-convert
                  '((label ((hiphoppop (id tok foo) #f
                                       (foo bar)
                                       (:= hiphop (pop))
                                       (push foo)
                                       (push bar)
                                       (accept))
                            (indirection () plus-sign
                                         ()
                                         (go hiphoppop
                                             (nterm kanye)
                                             (nterm jay-z))))
                           (go indirection))))
                 (make-pdarisc
                  16
                  (list (uninitialized-pda-term
                         (make-label
                          8
                          (list (uninitialized-label-name 'hiphoppop)
                                (uninitialized-label-name 'indirection))
                          '((id tok foo) ())
                          '(#f plus-sign)
                          `((,(uninitialized-register 'foo)
                             ,(uninitialized-register 'bar))
                            ())
                          (list
                           (list
                            (uninitialized-pda-term (join-point 15
                                                                (uninitialized-label-name 'hiphoppop)
                                                                `(,(uninitialized-register 'foo)
                                                                  ,(uninitialized-register 'bar))))
                            (uninitialized-pda-term (make-assign 9
                                                                 (uninitialized-register 'hiphop)
                                                                 (make-pop)))
                            (uninitialized-pda-term (make-push 10 (uninitialized-register 'foo)))
                            (uninitialized-pda-term (make-push 11 (uninitialized-register 'bar)))
                            (uninitialized-pda-term (make-accept 12 '())))
                           (list
                            (uninitialized-pda-term (join-point 16
                                                                (uninitialized-label-name 'indirection)
                                                                '()))
                            (uninitialized-pda-term (make-go
                                                     13
                                                     (uninitialized-label-name 'hiphoppop)
                                                     (list
                                                      (make-nterm 'kanye)
                                                      (make-nterm 'jay-z))))))
                          (list (uninitialized-pda-term (make-go
                                                         14
                                                         (uninitialized-label-name 'indirection)
                                                         (list)))))))))))
(require rackunit/text-ui)
(run-tests from-risc-tests)



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
