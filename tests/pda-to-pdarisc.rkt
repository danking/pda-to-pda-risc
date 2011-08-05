#lang racket
(require rackunit
         #;"example-pdas.rkt")

(require/expose "../pda-to-pdarisc.rkt"
                (convert-pda convert-rule convert-state categorize-actions))

(define pda1
  '((tokens A B $eos)
    (state s1 (shift A s2) (goto start s6))
    (state s2 (shift A s2) (shift B s3) (goto start s4))
    (state s3 (reduce #t r2))
    (state s4 (shift B s5))
    (state s5 (reduce #t r1))
    (state s6 (accept $eos))
    (rule r1 start  (#f #t #f) (lambda (x) (+ 2 x)))
    (rule r2 start  (#f #f)    (lambda () 2))
    (rule r3 accept (#t #f)    (lambda (x) x))))

(convert-pda pda1)

(for-each (lambda (in expected)
            (check-equal? (convert-rule in) expected))
          '((rule r1 start (#f #t #f) (lambda (x) (+ 2 x)))
            (rule r2 start (#f #f) (lambda () 2))
            (rule r3 accept (#t #f) (lambda (x) x)))
          '((r1
             ()
             (:= v1 (pop))
             (pop)
             (:= v2 (pop))
             (pop)
             (:= v3 (pop))
             (pop)
             (semantic-action (v2) (result) (lambda (x) (+ 2 x)))
             (:= return-here (pop))
             (go return-here (nterm start) result))
            (r2
             ()
             (:= v1 (pop))
             (pop)
             (:= v2 (pop))
             (pop)
             (semantic-action () (result) (lambda () 2))
             (:= return-here (pop))
             (go return-here (nterm start) result))
            (r3
             ()
             (:= v1 (pop))
             (pop)
             (:= v2 (pop))
             (pop)
             (semantic-action (v1) (result) (lambda (x) x))
             (:= return-here (pop))
             (go return-here (nterm accept) result))))
