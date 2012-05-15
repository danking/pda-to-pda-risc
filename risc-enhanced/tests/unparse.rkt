#lang racket
(require "../../parse-pdarisc.rkt"
         "../decorate.rkt"
         rackunit
         "../unparse.rkt")

(define parse/convert/decorate/unparse
  (compose unparse decorate parse-pdarisc))

(check-equal? (parse/convert/decorate/unparse
               '((:= foo (pop))
                 (:= one foo)
                 (:= 我 (state state-1))
                 (:= eat (nterm program))
                 (:= pie (current-token))
                 (:= _ (pop))
                 (accept)))
              '((:= (register 0 foo) (pop))
                (:= (register 1 one) (register 0 foo))
                (:= (register 2 我) (state state-1))
                (:= (register 3 eat) (nterm program))
                (:= (register 4 pie) (current-token))
                (:= (register 5 _) (pop))
                (accept)))

(check-equal? (parse/convert/decorate/unparse
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
              '((label (((label-name 6 hiphoppop) : (id tok foo) #f
                                                  ((register #f foo) (register #f bar))
                                                  (join-point (label-name 6 hiphoppop)
                                                              (register 8 foo)
                                                              (register 9 bar))
                                                  (:= (register 10 hiphop) (pop))
                                                  (push (register 8 foo))
                                                  (push (register 9 bar))
                                                  (accept))
                        ((label-name 7 indirection) : () plus-sign
                                                    ()
                                                    (join-point (label-name 7 indirection))
                                                    (go (label-name 6 hiphoppop)
                                                        (nterm kanye)
                                                        (nterm jay-z))))
                       (go (label-name 7 indirection)))))
