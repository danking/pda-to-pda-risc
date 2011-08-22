#lang racket
(require "../pda-data.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAMPLE DATA USED IN TESTS
(define pda1
  '((tokens A B $eos)
    (eos $eos)
    (start s1)
    (state s1 (shift A s2) (goto start s6))
    (state s2 (shift A s2) (shift B s3) (goto start s4))
    (state s3 (reduce #t r2))
    (state s4 (shift B s5))
    (state s5 (reduce #t r1))
    (state s6 (accept $eos))
    (rule r1 start  3 (+ 2 v2))
    (rule r2 start  2 2)
    (rule r3 accept 2 v1)))
(define pda1-risc-struct
  (make-pda '(A B $eos)
            '$eos
            's1
            (list (make-state 's6 '((accept $eos)) '())
                  (make-state 's5 '((reduce #t r1)) '())
                  (make-state 's4 '((shift B s5)) '())
                  (make-state 's3 '((reduce #t r2)) '())
                  (make-state 's2
                              '((shift A s2) (shift B s3))
                              '((goto start s4)))
                  (make-state 's1 '((shift A s2)) '((goto start s6))))
            (list (make-rule 'r3 'accept 2 'v1)
                  (make-rule 'r2 'start 2 '2)
                  (make-rule 'r1 'start 3 '(+ 2 v2)))
            (hasheq)))

(define pda1-risc
  '(label ((s1 ()
               (if-eos (go s1-eos)
                       (block get-token
                              (go s1-have-token))))
           (s1-eos ())
           (s1-have-token ()
                          (token-case
                           (A (block (push (state s1))
                                     (push (current-token))
                                     drop-token
                                     (go s2)))))
           (s2 ()
               (if-eos (go s2-eos)
                       (block get-token
                              (go s2-have-token))))
           (s2-eos ())
           (s2-have-token ()
                          (token-case
                           (A (block (push (state s2))
                                     (push (current-token))
                                     drop-token
                                     (go s2)))
                           (B (block (push (state s2))
                                     (push (current-token))
                                     drop-token
                                     (go s3)))))

           (s3 ()
               (if-eos (go s3-eos)
                       (block get-token
                              (go s3-have-token))))
           (s3-eos ()
                   (go r2-eos))
           (s3-have-token ()
                          (token-case
                           (#t (go r2))))

           (s4 ()
               (if-eos (go s4-eos)
                       (block get-token
                              (go s4-have-token))))
           (s4-eos ())
           (s4-have-token ()
                          (token-case
                           (B (block (push (state s4))
                                     (push (current-token))
                                     drop-token
                                     (go s5)))))

           (s5 ()
               (if-eos (go s5-eos)
                       (block get-token
                              (go s5-have-token))))
           (s5-eos () (go r1-eos))
           (s5-have-token ()
                          (token-case
                           (#t (go r1))))

           (s6 ()
               (if-eos (go s6-eos)
                       (block get-token
                              (go s6-have-token))))
           (s6-eos ()
                   (block (:= final-semantic-value (pop))
                          (accept final-semantic-value)))
           (s6-have-token () (token-case))

           (r1 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (:= v3 (pop))
               (:= st3 (pop))
               (semantic-action (v1 v2 v3) (result) (+ 2 v2))
               (push st3)
               (push result)
               (state-case st3
                           (s1 (go s6-have-token))
                           (s2 (go s4-have-token))))

           (r1-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (:= v3 (pop))
                   (:= st3 (pop))
                   (semantic-action (v1 v2 v3) (result) (+ 2 v2))
                   (push st3)
                   (push result)
                   (state-case st3
                               (s1 (go s6-eos))
                               (s2 (go s4-eos))))

           (r2 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (semantic-action (v1 v2) (result) 2)
               (push st2)
               (push result)
               (state-case st2
                           (s1 (go s6-have-token))
                           (s2 (go s4-have-token))))

           (r2-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (semantic-action (v1 v2) (result) 2)
                   (push st2)
                   (push result)
                   (state-case st2
                               (s1 (go s6-eos))
                               (s2 (go s4-eos))))
           (r3 ()
               (:= v1 (pop))
               (:= st1 (pop))
               (:= v2 (pop))
               (:= st2 (pop))
               (semantic-action (v1 v2) (result) v1)
               (push st2)
               (push result)
               (state-case st2))

           (r3-eos ()
                   (:= v1 (pop))
                   (:= st1 (pop))
                   (:= v2 (pop))
                   (:= st2 (pop))
                   (semantic-action (v1 v2) (result) v1)
                   (push st2)
                   (push result)
                   (state-case st2)))
       (go s1)))