#lang racket
(require rackunit
         "../pdarisc-data.rkt"
         "../unparse-pdarisc.rkt"
         "../parse-pdarisc.rkt")

(define-check (check-unparser pdarisc)
  (let ((actual (unparse-pdarisc (parse-pdarisc pdarisc))))
    (unless (equal? actual pdarisc)
      (with-check-info (('actual actual)) (fail-check)))))

(define-test-suite
  unparse-pdarisc-tests
  (test-case
   "assignments"
   (check-unparser '((:= foo (pop))
                     (:= one two)
                     (:= 我 (state state-1))
                     (:= eat (nterm program))
                     (:= pie (current-token))
                     (:= _ (pop))
                     (accept))))
  (test-case
   "accept/reject"
   (check-unparser '((push me)
                     (push (state around))
                     (accept)))
   (check-unparser '((accept foo bar)))
   (check-unparser '((reject))))
  (test-case
   "sem act"
   (check-unparser '((semantic-action sem-act-name
                                      (exp exps)
                                      (yahoo #f)
                                      '(values (cons exp exps)
                                               'nothin-to-see-here))
                     (stack-ensure 3)
                     (accept))))
  (test-case
   "block"
   (check-unparser '((block (:= foo (pop))
                            drop-token
                            get-token)
                     (accept)))
   (check-unparser '((block (:= foo (pop))
                            drop-token
                            get-token
                            (accept)))))
  (test-case
   "label"
   (check-unparser '((label ((hiphoppop (id tok foo) #f
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
  (test-case
   "if-eos"
   (check-unparser '((if-eos (go secret-stuff)
                             (block get-token drop-token (go foo))))))
  (test-case
   "state-case"
   (check-unparser '((state-case what-am-i
                                 (pink (go pink-stuff))
                                 (blue drop-token (go azure))
                                 (green (go soylent!))))))
  (test-case
   "token-case"
   (check-unparser '((token-case
                      (small (push (current-token)) (go tiny teeny weeny))
                      (big drop-token (go big!))
                      (home (:= you out) (go home)))))))

(require rackunit/text-ui)
(run-tests unparse-pdarisc-tests)
