#lang racket
(require racket/enter
         rackunit
         "example-pdas.rkt")

(require/expose "../pda-to-pda0.rkt"
                (convert-pda convert-rule convert-state categorize-actions))

;; convert-pda
(check-equal? (convert-pda pda1)
              pda1-risc)
(check-equal? (convert-pda pda2)
              pda2-risc)

;; convert-rule
(map (lambda (in expected)
       (check-equal? (convert-rule in 'accept-block) expected))
     '((rule r1 start (#f #f)    1)
       (rule r2 start (#f #f #f) 2)
       (rule r3 foo   (#f)       3)
       (rule r4 foo   (#f #f)    4))
     '((block r1 #f #f
              (reduce r1)
              (pop-states 2)
              return)
       (block r2 #f #f
              (reduce r2)
              (pop-states 3)
              return)
       (block r3 #f #f
              (reduce r3)
              (pop-states 1)
              return)
       (block r4 #f #f
              (reduce r4)
              (pop-states 2)
              return)))

;; convert-state
(map (lambda (in expected)
       (let ((result (convert-state in 'accept-block)))
         (check-equal? result expected)))
     '((state s1
              (shift (A) s2)
              (goto start s8)
              (goto foo s3))
       (state s2
              (reduce () r3))
       (state s3
              (shift (A) s4)
              (shift (C) s5)
              (shift (B) s6)))
     '(((block s1 #f #f
               push-token
               (jump s1-body))
        (block s1-body #f #f
               (push-state s1-reduce)
               (lcase ((A) s2)))
        (block s1-reduce #f #f
               (push-state s1-reduce)
               (gcase (start s8-body)
                      (foo s3-body))))
       ((block s2 #f #f
               push-token
               (jump s2-body))
        (block s2-body #f #f
               (push-state s2-reduce)
               (lcase (() r3)))
        (block s2-reduce #f #f
               (push-state s2-reduce)
               (gcase)))
       ((block s3 #f #f
               push-token
               (jump s3-body))
        (block s3-body #f #f
               (push-state s3-reduce)
               (lcase ((A) s4)
                      ((C) s5)
                      ((B) s6)))
        (block s3-reduce #f #f
               (push-state s3-reduce)
               (gcase)))))

;; categorize-actions
(let-values (((gotos actions) (categorize-actions '((shift (A) s2)
                                                    (goto start s8)
                                                    (goto foo s3)))))
  (check-equal? gotos '((goto start s8) (goto foo s3)))
  (check-equal? actions '((shift (A) s2))))
(let-values (((gotos actions) (categorize-actions '((shift (A) s4)
                                                    (shift (C) s5)
                                                    (shift (B) s6)))))
  (check-equal? gotos '())
  (check-equal? actions '((shift (A) s4) (shift (C) s5) (shift (B) s6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A sample grammar that I represented in pseudo-BNF, CFG lang, and PDA lang

#|
accept ::= start $end
start ::= foo C
        | foo B foo
foo ::= A
      | foo A

CFG from doc.txt
((tokens A B C)
 (non-term start
           (=> (foo C) 0)
           (=> (foo B foo) 0))
 (non-term foo
           (=> (A) 0)
           (=> (foo A) 0)))

PDA from doc.txt
((tokens A B C $end)
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
 (rule r5 accept (#f #f)    5))
|#
