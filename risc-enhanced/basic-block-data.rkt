#lang racket

(provide (all-defined-out))

(struct basic-block (uid nodes (succ #:mutable))
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc a b recur)
           (recur (basic-block-uid a) (basic-block-uid b)))
         (define (hash-proc a recur)
           (recur (basic-block-uid a)))
         (define (hash2-proc a recur)
           (recur (basic-block-uid a)))])

(define (new-basic-block uid terms succ)
  (basic-block uid terms succ))

(struct branch-block (uid node (succs #:mutable))
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc a b recur)
           (recur (branch-block-uid a) (branch-block-uid b)))
         (define (hash-proc a recur)
           (recur (branch-block-uid a)))
         (define (hash2-proc a recur)
           (recur (branch-block-uid a)))])

(define (new-branch-block uid terms succs)
  (branch-block uid terms succs))

(define (block-set/add-succ! block succ)
  (cond [(basic-block? block) (set-basic-block-succ! block succ)]
        [else (set-branch-block-succs! block
                                       (set-add (branch-block-succs block)
                                                succ))]))

(define (block-succs block)
  (cond [(basic-block? block)
         (if (basic-block-succ block)
             (set (basic-block-succ block))
             (set))]
        [else (branch-block-succs block)]))

(define (block-nodes block)
  (cond [(basic-block? block) (basic-block-nodes block)]
        [else (list (branch-block-node block))]))

(define (block-uid block)
  (cond [(basic-block? block) (basic-block-uid block)]
        [else (branch-block-uid block)]))

(define basic-block/c
  (struct/c basic-block natural-number/c (listof any/c) any/c))
(define branch-block/c
  (struct/c branch-block natural-number/c any/c (set/c any/c)))

(define block/c (or/c basic-block/c branch-block/c))
