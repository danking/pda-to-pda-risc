#lang racket
(require "../parse-pda.rkt"
         "../pdarisc-data.rkt"
         "../parse-pdarisc.rkt"
         "check-syntax-equal.rkt"
         (rename-in rackunit
                    (test-case rackunit-test-case*)))
(require/expose "../pdarisc-node-graph.rkt"
                (build-node-graph reg-node join-node reset-key-counter))

(define-syntax-rule (test-case name . body)
  (rackunit-test-case* name (reset-key-counter) . body))

(define/provide-test-suite pdarisc-node-graph
  (test-case
   "simple linear program"
   (check-equal? (build-node-graph (parse-pdarisc '((:= r (state foo))
                                                     (push (nterm lala))
                                                     get-token
                                                     (accept r))))
                 (hash 0
                       (reg-node '(1) (parse-insn '(:= r (state foo))))
                       1
                       (reg-node '(2) (parse-insn '(push (nterm lala))))
                       2
                       (reg-node '(3) (parse-insn 'get-token))
                       3
                       (reg-node '() (parse-insn* '(accept r))))))

  (test-case
   "linear program with a block"
   (check-equal? (build-node-graph (parse-pdarisc '((:= r (state foo))
                                                     (block (push (nterm lala))
                                                            get-token)
                                                     (accept r))))
                 (hash 0
                       (reg-node '(1) (parse-insn '(:= r (state foo))))
                       1
                       (reg-node '(2) (parse-insn '(push (nterm lala))))
                       2
                       (reg-node '(3) (parse-insn 'get-token))
                       3
                       (reg-node '() (parse-insn* '(accept r))))))

  (test-case
   "if-eos branching pdarisc"
   (check-equal? (build-node-graph (parse-pdarisc '((:= r (state foo))
                                                     (if-eos (block (push (nterm lala))
                                                                    (reject))
                                                             (block get-token
                                                                    (accept r))))))
                 (hash 0
                       (reg-node '(1) (parse-insn '(:= r (state foo))))
                       1
                       (reg-node '(2 3)
                                 (parse-insn* '(if-eos (block (push (nterm lala))
                                                             (reject))
                                                      (block get-token
                                                             (accept r)))))
                       2
                       (reg-node '(4) (parse-insn '(push (nterm lala))))
                       4
                       (reg-node '() (parse-insn* '(reject)))
                       3
                       (reg-node '(5) (parse-insn 'get-token))
                       5
                       (reg-node '() (parse-insn* '(accept r))))))

  (test-case
   "state-case branching pdarisc"
   (check-equal? (build-node-graph (parse-pdarisc '((:= r (state foo))
                                                     (state-case reg
                                                                 (a (push (nterm lala))
                                                                    (reject))
                                                                 (b get-token
                                                                    (accept r))
                                                                 (c (stack-ensure 5)
                                                                    get-token
                                                                    drop-token
                                                                    (reject))))))
                 (hash 0
                       (reg-node '(1) (parse-insn '(:= r (state foo))))
                       1
                       (reg-node '(2 3 4)
                                 (parse-insn* '(state-case reg
                                                           (a (push (nterm lala))
                                                              (reject))
                                                           (b get-token
                                                              (accept r))
                                                           (c (stack-ensure 5)
                                                              get-token
                                                              drop-token
                                                              (reject)))))
                       2
                       (reg-node '(5) (parse-insn '(push (nterm lala))))
                       5
                       (reg-node '() (parse-insn* '(reject)))

                       3
                       (reg-node '(6) (parse-insn 'get-token))
                       6
                       (reg-node '() (parse-insn* '(accept r)))

                       4
                       (reg-node '(7)  (parse-insn '(stack-ensure 5)))
                       7
                       (reg-node '(8)  (parse-insn 'get-token))
                       8
                       (reg-node '(9)  (parse-insn 'drop-token))
                       9
                       (reg-node '()  (parse-insn* '(reject))))))
  (test-case
   "labeled code points pdarisc"
   (check-syntax-equal? (build-node-graph
                         (parse-pdarisc `((label ((,#'a : () #f
                                                        ()
                                                        (:= r1 (nterm lala))
                                                        (go ,#'b r1))
                                                  (,#'b : () #f
                                                        (arg1)
                                                        get-token
                                                        (accept r))
                                                  (,#'c : () #f
                                                        (arg1 arg2)
                                                        (stack-ensure 5)
                                                        (reject)))
                                                 (go ,#'a)))))
                        (hash 0
                              (reg-node '(4)
                                        (parse-insn*
                                         `(label ((,#'a : () #f
                                                     ()
                                                     (:= r1 (nterm lala))
                                                     (go ,#'b r1))
                                                  (,#'b : () #f
                                                     (arg1)
                                                     get-token
                                                     (accept r))
                                                  (,#'c : () #f
                                                     (arg1 arg2)
                                                     (stack-ensure 5)
                                                     (reject)))
                                                 (go ,#'a))))
                              1
                              (join-node '(5) '())
                              5
                              (reg-node '(6) (parse-insn '(:= r1 (nterm lala))))
                              6
                              (reg-node '(2) (parse-insn* `(go ,#'b r1)))

                              2
                              (join-node '(7) (list (named-reg 'arg1)))
                              7
                              (reg-node '(8) (parse-insn 'get-token))
                              8
                              (reg-node '() (parse-insn* '(accept r)))

                              3
                              (join-node '(9) (list (named-reg 'arg1) (named-reg 'arg2)))
                              9
                              (reg-node '(10) (parse-insn '(stack-ensure 5)))
                              10
                              (reg-node '() (parse-insn* '(reject)))

                              4
                              (reg-node '(1) (parse-insn* `(go ,#'a)))))))

(require rackunit/text-ui)

(run-tests pdarisc-node-graph)