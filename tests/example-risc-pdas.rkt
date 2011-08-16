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
    (rule r1 start  3 (+ 2 x))
    (rule r2 start  2 2)
    (rule r3 accept 2 x)))
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
            (list (make-rule 'r3 'accept 2 'x)
                  (make-rule 'r2 'start 2 '2)
                  (make-rule 'r1 'start 3 '(+ 2 x)))
            (hasheq)))

(define single-shift-state
  '(state s1 (shift A s2) (goto start s6)))
(define single-shift-state-risc
  '((s1 ()
        (push (state s1-reduce))
        (if-eos (block)
                (block get-token
                       (push (current-token))
                       (token-case (A (go s2))))))
    (s1-reduce (nt sem-val)
               (push (state s1-reduce))
               (push sem-val)
               (state-case nt (start (go s6))))
    (s1-eos ()
            (push (state s1-reduce-eos)))
    (s1-reduce-eos (nt sem-val)
                   (push (state s1-reduce-eos))
                   (push sem-val)
                   (state-case nt (start (go s6-eos))))))
(define multiple-shift-state
  '(state s2 (shift A s2) (shift B s3) (goto start s4)))
(define multiple-shift-state-risc
  '((s2 ()
        (push (state s2-reduce))
        (if-eos (block)
                (block get-token
                       (push (current-token))
                       (token-case (A (go s2))
                                   (B (go s3))))))
    (s2-reduce (nt sem-val)
               (push (state s2-reduce))
               (push sem-val)
               (state-case nt (start (go s4))))
    (s2-eos ()
            (push (state s2-reduce-eos)))
    (s2-reduce-eos (nt sem-val)
                   (push (state s2-reduce-eos))
                   (push sem-val)
                   (state-case nt (start (go s4-eos))))))
(define reducing-state
  '(state s3 (reduce () r2)))
(define reducing-state-risc
  '((s3 ()
        (push (state s3-reduce))
        (if-eos (block)
                (block get-token
                       (push (current-token))
                       (token-case (#f (go r2))))))
    (s3-reduce (nt sem-val)
               (push (state s3-reduce))
               (push sem-val)
               (state-case nt))
    (s3-eos ()
            (push (state s3-reduce-eos)))
    (s3-reduce-eos (nt sem-val)
                   (push (state s3-reduce-eos))
                   (push sem-val)
                   (state-case nt))))
(define accepting-state
  '(state s6 (accept $eos)))
(define accepting-state-risc
  '((s6 ()
         (push (state s6-reduce))
         (if-eos (block (block (pop)
                               (:= return-value (pop))
                               (accept return-value)))
                 (block get-token
                        (push (current-token))
                        (token-case))))
     (s6-reduce (nt sem-val)
                (push (state s6-reduce))
                (push sem-val)
                (state-case nt))
     (s6-eos ()
             (push (state s6-reduce-eos))
             (block (pop)
                    (:= return-value (pop))
                    (accept return-value)))
     (s6-reduce-eos (nt sem-val)
                    (push (state s6-reduce-eos))
                    (push sem-val)
                    (state-case nt))))

(define 3-rhs-rule
  '(rule r1 non-terminal 3 (+ 2 x)))
(define 3-rhs-rule-risc
  '(r1
    ()
    (:= v1 (pop))
    (pop)
    (:= v2 (pop))
    (pop)
    (:= v3 (pop))
    (pop)
    (semantic-action (v2) (result) (+ 2 x))
    (:= return-here (pop))
    (go return-here (nterm non-terminal) result)))
(define 2-rhs-rule
  '(rule r3 non-terminal 2 x))
(define 2-rhs-rule-risc
  '(r3
    ()
    (:= v1 (pop))
    (pop)
    (:= v2 (pop))
    (pop)
    (semantic-action (v1) (result) x)
    (:= return-here (pop))
    (go return-here (nterm non-terminal) result)))
(define 2-rhs-rule-no-bindings
  '(rule r2 non-terminal 2 2))
(define 2-rhs-rule-no-bindings-risc
  '(r2
    ()
    (:= v1 (pop))
    (pop)
    (:= v2 (pop))
    (pop)
    (semantic-action () (result) 2)
    (:= return-here (pop))
    (go return-here (nterm non-terminal) result)))


(define pda1-risc
  '(label
    ((s1
      ()
      (push (state s1-reduce))
      (if-eos
       (block)
       (block get-token (push (current-token)) (token-case (A (go s2))))))
     (s1-reduce
      (nt sem-val)
      (push (state s1-reduce))
      (push sem-val)
      (state-case nt (start (go s6))))
     (s1-eos () (push (state s1-reduce-eos)))
     (s1-reduce-eos
      (nt sem-val)
      (push (state s1-reduce-eos))
      (push sem-val)
      (state-case nt (start (go s6-eos))))
     (s2
      ()
      (push (state s2-reduce))
      (if-eos
       (block)
       (block
        get-token
        (push (current-token))
        (token-case (A (go s2)) (B (go s3))))))
     (s2-reduce
      (nt sem-val)
      (push (state s2-reduce))
      (push sem-val)
      (state-case nt (start (go s4))))
     (s2-eos () (push (state s2-reduce-eos)))
     (s2-reduce-eos
      (nt sem-val)
      (push (state s2-reduce-eos))
      (push sem-val)
      (state-case nt (start (go s4-eos))))
     (s3
      ()
      (push (state s3-reduce))
      (if-eos
       (block)
       (block get-token (push (current-token)) (token-case (#t (go r2))))))
     (s3-reduce
      (nt sem-val)
      (push (state s3-reduce))
      (push sem-val)
      (state-case nt))
     (s3-eos
      ()
      (push (state s3-reduce-eos))
      (go r2-eos))
     (s3-reduce-eos
      (nt sem-val)
      (push (state s3-reduce-eos))
      (push sem-val)
      (state-case nt))
     (s4
      ()
      (push (state s4-reduce))
      (if-eos
       (block)
       (block get-token (push (current-token)) (token-case (B (go s5))))))
     (s4-reduce
      (nt sem-val)
      (push (state s4-reduce))
      (push sem-val)
      (state-case nt))
     (s4-eos () (push (state s4-reduce-eos)))
     (s4-reduce-eos
      (nt sem-val)
      (push (state s4-reduce-eos))
      (push sem-val)
      (state-case nt))
     (s5
      ()
      (push (state s5-reduce))
      (if-eos
       (block)
       (block get-token (push (current-token)) (token-case (#t (go r1))))))
     (s5-reduce
      (nt sem-val)
      (push (state s5-reduce))
      (push sem-val)
      (state-case nt))
     (s5-eos
      ()
      (push (state s5-reduce-eos))
      (go r1-eos))
     (s5-reduce-eos
      (nt sem-val)
      (push (state s5-reduce-eos))
      (push sem-val)
      (state-case nt))
     (s6
      ()
      (push (state s6-reduce))
      (if-eos
       (block (block (pop) (:= return-value (pop)) (accept return-value)))
       (block get-token (push (current-token)) (token-case))))
     (s6-reduce
      (nt sem-val)
      (push (state s6-reduce))
      (push sem-val)
      (state-case nt))
     (s6-eos
      ()
      (push (state s6-reduce-eos))
      (block (pop) (:= return-value (pop)) (accept return-value)))
     (s6-reduce-eos
      (nt sem-val)
      (push (state s6-reduce-eos))
      (push sem-val)
      (state-case nt))
     (r1
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (:= v3 (pop))
      (pop)
      (semantic-action (v1 v2 v3) (result) (+ 2 x))
      (:= reduce-to (pop))
      (state-case reduce-to
        (s2-reduce (go s2-reduce (nterm start) result))
        (s1-reduce (go s1-reduce (nterm start) result))))
     (r1-eos
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (:= v3 (pop))
      (pop)
      (semantic-action (v1 v2 v3) (result) (+ 2 x))
      (:= reduce-to (pop))
      (state-case reduce-to
        (s2-reduce (go s2-reduce-eos (nterm start) result))
        (s1-reduce (go s1-reduce-eos (nterm start) result))
        (s2-reduce-eos (go s2-reduce-eos (nterm start) result))
        (s1-reduce-eos (go s1-reduce-eos (nterm start) result))))
     (r2
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (semantic-action (v1 v2) (result) 2)
      (:= reduce-to (pop))
      (state-case reduce-to
        (s2-reduce (go s2-reduce (nterm start) result))
        (s1-reduce (go s1-reduce (nterm start) result))))
     (r2-eos
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (semantic-action (v1 v2) (result) 2)
      (:= reduce-to (pop))
      (state-case reduce-to
        (s2-reduce (go s2-reduce-eos (nterm start) result))
        (s1-reduce (go s1-reduce-eos (nterm start) result))
        (s2-reduce-eos (go s2-reduce-eos (nterm start) result))
        (s1-reduce-eos (go s1-reduce-eos (nterm start) result))))
     (r3
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (semantic-action (v1 v2) (result) x)
      (:= reduce-to (pop))
      (state-case reduce-to))
     (r3-eos
      ()
      (:= v1 (pop))
      (pop)
      (:= v2 (pop))
      (pop)
      (semantic-action (v1 v2) (result) x)
      (:= reduce-to (pop))
      (state-case reduce-to)))
    (go s1)))
