#lang racket
(require rackunit
         "../../parse-pdarisc.rkt"
         "../from-risc.rkt"
         "../data.rkt"
         "../set-uids.rkt"
         "../set-preds.rkt")

(define parse/convert/set-uids (compose set-uids convert/pdarisc parse-pdarisc))

(define pda1
  (parse/convert/set-uids '((:= foo (pop))
                            (:= one foo)
                            (:= 我 (state state-1))
                            (:= eat (nterm program))
                            (:= pie (current-token))
                            (:= _ (pop))
                            (accept))))

(set-preds! pda1)
(pretty-print pda1)

(define pda2
  (parse/convert/set-uids
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
            (go indirection)))))

(set-preds! pda2)
(pretty-print pda2)
