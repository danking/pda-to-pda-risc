#lang racket
(require "../pda-to-pda0.rkt")

;; accept -> start $end
;; start -> foo C
;;        | foo B foo
;; foo -> A
;;      | foo A

;; CFG from doc.txt
;; ((tokens A B C)
;;  (non-term start
;;            (=> (foo C) 0)
;;            (=> (foo B foo) 0))
;;  (non-term foo
;;            (=> (A) 0)
;;            (=> (foo A) 0)))

;; PDA from doc.txt
;; ((tokens A B C)
;;  (state s1
;;         (shift (A) s2)
;;         (goto start s8)
;;         (goto foo s3))
;;  (state s2
;;         (reduce () r3))
;;  (state s3
;;         (shift (A) s4)
;;         (shift (C) s5)
;;         (shift (B) s6))
;;  (state s4
;;         (reduce () r4))
;;  (state s5
;;         (reduce () r1))
;;  (state s6
;;         (shift (A) s2)
;;         (goto foo s7))
;;  (state s7
;;         (shift (A) s4)
;;         (reduce () r2))
;;  (state s8
;;         (accept ($end)))

;;  (rule r1 start  (#f #f)    1)
;;  (rule r2 start  (#f #f #f) 2)
;;  (rule r3 foo    (#f)       3)
;;  (rule r4 foo    (#f #f)    4)
;;  (rule r5 accept (#f #f)    5))

;; convert-rule
(map (lambda (in expected)
       (equal? (convert-rule in 'accept-block) expected))
     '((rule r1 start (#f #f)    1)
       (rule r2 start (#f #f #f) 2)
       (rule r3 foo   (#f)       3)
       (rule r4 foo   (#f #f)    4))
     '((block r1 #f #f
              (reduce r1)
              (pop-states 2)
              (jump (top-of-state-stack)))
       (block r2 #f #f
              (reduce r2)
              (pop-states 3)
              (jump (top-of-state-stack)))
       (block r3 #f #f
              (reduce r3)
              (pop-states 1)
              (jump (top-of-state-stack)))
       (block r4 #f #f
              (reduce r4)
              (pop-states 2)
              (jump (top-of-state-stack)))))

;; convert-state
(map (lambda (in expected)
       (let ((result (convert-state in 'accept-block)))
        (or (equal? result expected)
            `(in: ,in result: ,result expected: ,expected))))
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
               (push-state s1-reduce)
               (lcase ((A) s2)))
        (block s1-reduce #f #f
               (gcase (start s8)
                      (foo s3))))
       ((block s2 #f #f
               (push-state s2-reduce)
               ; (push-token) when should this be done?
               (lcase (() r3)))
        (block s2-reduce #f #f
               (gcase)))
       ((block s3 #f #f
               (push-state s3-reduce)
               (lcase ((A) s4)
                      ((C) s5)
                      ((B) s6)))
        (block s3-reduce #f #f
               (gcase)))))

;; group-actions
(equal? (group-actions '((shift (A) s2)
                         (goto start s8)
                         (goto foo s3)))
        '(((goto start s8) (goto foo s3))
          ((shift (A) s2))))
(equal? (group-actions '((shift (A) s4)
                         (shift (C) s5)
                         (shift (B) s6)))
        '(()
          ((shift (A) s4) (shift (C) s5) (shift (B) s6))))


