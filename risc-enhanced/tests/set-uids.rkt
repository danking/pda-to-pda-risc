#lang racket
(require rackunit
         "../set-uids.rkt"
         "../from-risc.rkt"
         "../../parse-pdarisc.rkt"
         "../../../racket-utils/environment.rkt"
         "../data.rkt")

(define parse/convert/set-uids (compose set-uids convert/pdarisc parse-pdarisc))

(reset-id)

(let* ((foo-reg (uninitialized-register 'foo))
       (foo-term (uninitialized-pda-term foo-reg))
       (foo-reg (su/reg-def foo-reg foo-term empty-env))
       (new-env (extend-env/regs empty-env (list foo-reg))))
  (check-equal? foo-reg (register 'foo
                                      0
                                      foo-term
                                      (seteq)))
  (check-equal? new-env (hash 'foo foo-reg))
  (let* ((foo-reg2 (uninitialized-register 'foo))
         (foo-term2 (uninitialized-pda-term foo-reg2))
         (foo-reg2 (su/reg-use foo-reg2 foo-term2 new-env)))
    (check-eq? foo-reg2 foo-reg)
    (check-equal? (register-uses (hash-ref new-env 'foo))
                  (seteq foo-term2))
    (let* ((foo-reg3 (uninitialized-register 'foo))
           (foo-term3 (uninitialized-pda-term foo-reg3))
           (foo-reg3 (su/reg-use foo-reg3 foo-term3 new-env)))
      (check-eq? foo-reg3 foo-reg)
      (check-equal? (register-uses (hash-ref new-env 'foo))
                    (seteq foo-term2 foo-term3))
      (check-equal? (hash-ref new-env 'foo) foo-reg))))

(pretty-print
 (parse/convert/set-uids '((label ((hiphoppop (id tok foo) #f
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
                                  (go indirection)))))
