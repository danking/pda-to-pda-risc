#lang racket
(provide (all-defined-out))

(define pda1
  '((tokens A B $end)
    (state s1 (shift (A) s2) (goto start s6))
    (state s2 (shift (A) s2) (shift (B) s3) (goto start s4))
    (state s3 (reduce () r2))
    (state s4 (shift (B) s5))
    (state s5 (reduce () r1))
    (state s6 (accept ($end)))
    (rule r1 start  (#f #t #f) (lambda (x) (+ 2 x)))
    (rule r2 start  (#f #f)    (lambda () 2))
    (rule r3 accept (#t #f)    (lambda (x) x))))
(define pda1-risc
  '((tokens A B $end)
    (block s1 #f #f push-token (jump s1-body))
    (block s1-body #f #f (push-state s1-reduce) (lcase ((A) s2)))
    (block s1-reduce #f #f (push-state s1-reduce) (gcase (start s6-body)))

    (block s2 #f #f push-token (jump s2-body))
    (block s2-body #f #f (push-state s2-reduce) (lcase ((A) s2) ((B) s3)))
    (block s2-reduce #f #f (push-state s2-reduce) (gcase (start s4-body)))

    (block s3 #f #f push-token (jump s3-body))
    (block s3-body #f #f (push-state s3-reduce) (lcase (() r2)))
    (block s3-reduce #f #f (push-state s3-reduce) (gcase))

    (block s4 #f #f push-token (jump s4-body))
    (block s4-body #f #f (push-state s4-reduce) (lcase ((B) s5)))
    (block s4-reduce #f #f (push-state s4-reduce) (gcase))

    (block s5 #f #f push-token (jump s5-body))
    (block s5-body #f #f (push-state s5-reduce) (lcase (() r1)))
    (block s5-reduce #f #f (push-state s5-reduce) (gcase))

    (block s6 #f #f push-token (jump s6-body))
    (block s6-body #f #f (push-state s6-reduce) (lcase (($end) accept-block)))
    (block s6-reduce #f #f (push-state s6-reduce) (gcase))

    (block r1 #f #f (reduce r1) (pop-states 3) return)
    (rule r1 start (#f #t #f) (lambda (x) (+ 2 x)))
    (block r2 #f #f (reduce r2) (pop-states 2) return)
    (rule r2 start (#f #f) (lambda () 2))
    (block r3 #f #f (reduce r3) (pop-states 2) return)
    (rule r3 accept (#t #f) (lambda (x) x))


    (block accept-block #f #f accept)))
(define pda2
  '((tokens A B C $end)
    (state s1
           (shift (A) s2)
           (goto start s8)
           (goto foo s3))
    (state s2
           (reduce () r3))
    (state s3
           (shift (A) s4)
           (shift (C) s5)
           (shift (B) s6))
    (state s4
           (reduce () r4))
    (state s5
           (reduce () r1))
    (state s6
           (shift (A) s2)
           (goto foo s7))
    (state s7
           (shift (A) s4)
           (reduce () r2))
    (state s8
           (accept ($end)))

    (rule r1 start  (#f #f)    1)
    (rule r2 start  (#f #f #f) 2)
    (rule r3 foo    (#f)       3)
    (rule r4 foo    (#f #f)    4)
    (rule r5 accept (#f #f)    5)))
(define pda2-risc
  '((tokens A B C $end)
    (block s1 #f #f push-token (jump s1-body))
    (block s1-body #f #f
           (push-state s1-reduce)
           (lcase ((A) s2)))
    (block s1-reduce #f #f
           (push-state s1-reduce)
           (gcase (start s8-body)
                  (foo s3-body)))

    (block s2 #f #f push-token (jump s2-body))
    (block s2-body #f #f
           (push-state s2-reduce)
           (lcase (() r3)))
    (block s2-reduce #f #f
           (push-state s2-reduce)
           (gcase))

    (block s3 #f #f push-token (jump s3-body))
    (block s3-body #f #f
           (push-state s3-reduce)
           (lcase ((A) s4)
                  ((C) s5)
                  ((B) s6)))
    (block s3-reduce #f #f
           (push-state s3-reduce)
           (gcase))

    (block s4 #f #f push-token (jump s4-body))
    (block s4-body #f #f
           (push-state s4-reduce)
           (lcase (() r4)))
    (block s4-reduce #f #f
           (push-state s4-reduce)
           (gcase))

    (block s5 #f #f push-token (jump s5-body))
    (block s5-body #f #f
           (push-state s5-reduce)
           (lcase (() r1)))
    (block s5-reduce #f #f
           (push-state s5-reduce)
           (gcase))

    (block s6 #f #f push-token (jump s6-body))
    (block s6-body #f #f
           (push-state s6-reduce)
           (lcase ((A) s2)))
    (block s6-reduce #f #f
           (push-state s6-reduce)
           (gcase (foo s7-body)))

    (block s7 #f #f push-token (jump s7-body))
    (block s7-body #f #f
           (push-state s7-reduce)
           (lcase ((A) s4)
                  (() r2)))
    (block s7-reduce #f #f
           (push-state s7-reduce)
           (gcase))

    (block s8 #f #f push-token (jump s8-body))
    (block s8-body #f #f
           (push-state s8-reduce)
           (lcase (($end) accept-block)))
    (block s8-reduce #f #f
           (push-state s8-reduce)
           (gcase))

    (block r1 #f #f (reduce r1) (pop-states 2) return)
    (rule r1 start  (#f #f)    1)
    (block r2 #f #f (reduce r2) (pop-states 3) return)
    (rule r2 start  (#f #f #f) 2)
    (block r3 #f #f (reduce r3) (pop-states 1) return)
    (rule r3 foo    (#f)       3)
    (block r4 #f #f (reduce r4) (pop-states 2) return)
    (rule r4 foo    (#f #f)    4)
    (block r5 #f #f (reduce r5) (pop-states 2) return)
    (rule r5 accept (#f #f)    5)


    (block accept-block #f #f accept)))