#lang racket
(require "../parse-pda.rkt"
         "../pdarisc-data.rkt"
         "../parse-pdarisc.rkt"
         "../node-graph.rkt"
         "check-syntax-equal.rkt"
         rackunit)
(require/expose "../pdarisc-node-graph.rkt"
                (build-node-graph join-node))

(define-syntax check-node-graph
  (syntax-rules ()
    ((_ pdarisc uidmapping graph)
     (let ((ng (build-node-graph (parse-pdarisc pdarisc))))
       (check-syntax-equal? (node-graph-uidmapping ng)
                            uidmapping)
       (check-equal? (node-graph-graph ng)
                     graph)))))

(define/provide-test-suite pdarisc-node-graph
  (test-case
   "simple linear program"
   (check-node-graph '((:= r (state foo))
                       (push (nterm lala))
                       get-token
                       (accept r))
                     (hash 3
                           (parse-insn '(:= r (state foo)))
                           2
                           (parse-insn '(push (nterm lala)))
                           1
                           (parse-insn 'get-token)
                           0
                           (parse-insn* '(accept r)))
                     (hash 3
                           (set 2)
                           2
                           (set 1)
                           1
                           (set 0))))

  (test-case
   "linear program with a block"
   (check-node-graph '((:= r (state foo))
                       (block (push (nterm lala))
                              get-token)
                       (accept r))
                     (hash 3
                           (parse-insn '(:= r (state foo)))
                           2
                           (parse-insn '(push (nterm lala)))
                           1
                           (parse-insn 'get-token)
                           0
                           (parse-insn* '(accept r)))
                     (hash 3
                           (set 2)
                           2
                           (set 1)
                           1
                           (set 0))))

  (test-case
   "if-eos branching pdarisc"
   (check-node-graph '((:= r (state foo))
                       (if-eos (block (push (nterm lala))
                                      (reject))
                               (block get-token
                                      (accept r))))
                     (hash 3
                           (parse-insn 'get-token)
                           0
                           (parse-insn* '(reject))
                           5
                           (parse-insn '(:= r (state foo)))
                           2
                           (parse-insn* '(accept r))
                           4
                           (parse-insn* '(if-eos (block (push (nterm lala))
                                                        (reject))
                                                 (block get-token
                                                        (accept r))))
                           1
                           (parse-insn '(push (nterm lala))))
                     (hash 5
                           (set 4)
                           4
                           (set 3 1)
                           3
                           (set 2)
                           1
                           (set 0))))

  (test-case
   "state-case branching pdarisc"
   (check-node-graph '((:= r (state foo))
                       (state-case reg
                                   (a (push (nterm lala))
                                      (reject))
                                   (b get-token
                                      (accept r))
                                   (c (stack-ensure 5)
                                      get-token
                                      drop-token
                                      (reject))))
                     (hash 9
                           (parse-insn '(:= r (state foo)))
                           8
                           (parse-insn* '(state-case reg
                                                     (a (push (nterm lala))
                                                        (reject))
                                                     (b get-token
                                                        (accept r))
                                                     (c (stack-ensure 5)
                                                        get-token
                                                        drop-token
                                                        (reject))))
                           7
                           (parse-insn '(stack-ensure 5))
                           6
                           (parse-insn 'get-token)
                           5
                           (parse-insn 'drop-token)
                           4
                           (parse-insn* '(reject))
                           3
                           (parse-insn 'get-token)
                           2
                           (parse-insn* '(accept r))
                           1
                           (parse-insn '(push (nterm lala)))
                           0
                           (parse-insn* '(reject)))
                     (hash 9
                           (set 8)
                           8
                           (set 1 3 7)
                           7
                           (set 6)
                           6
                           (set 5)
                           5
                           (set 4)
                           3
                           (set 2)
                           1
                           (set 0))))
  (test-case
   "labeled code points pdarisc"
   (check-node-graph `((label ((,#'a : () #f
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
                              (go ,#'a)))
                     (hash
                      0
                      (parse-insn* `(go ,#'b r1))
                      1
                      (parse-insn '(:= r1 (nterm lala)))
                      2
                      (join-node '())
                      3
                      (parse-insn* '(accept r))
                      4
                      (parse-insn 'get-token)
                      5
                      (join-node (list (named-reg 'arg1)))
                      6
                      (parse-insn* '(reject))
                      7
                      (parse-insn '(stack-ensure 5))
                      8
                      (join-node (list (named-reg 'arg1) (named-reg 'arg2)))
                      9
                      (parse-insn* `(go ,#'a)))
                     (hash 0
                           (set '#&5)
                           1
                           (set 0)
                           2
                           (set 1)
                           4
                           (set 3)
                           5
                           (set 4)
                           7
                           (set 6)
                           8
                           (set 7)
                           9
                           (set '#&2)))))

(require rackunit/text-ui)

(run-tests pdarisc-node-graph)