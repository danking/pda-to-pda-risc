#lang racket

(provide (all-defined-out))

(struct basic-block (nodes (succ #:mutable))
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc a b recur)
           (recur (basic-block-nodes a) (basic-block-nodes b)))
         (define (hash-proc a recur)
           (recur (basic-block-nodes a)))
         (define (hash2-proc a recur)
           (recur (basic-block-nodes a)))])

(define (new-basic-block terms succ)
  (basic-block terms succ))

(struct branch-block (node (succs #:mutable))
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc a b recur)
           (recur (branch-block-node a) (branch-block-node b)))
         (define (hash-proc a recur)
           (recur (branch-block-node a)))
         (define (hash2-proc a recur)
           (recur (branch-block-node a)))])

(define (new-branch-block terms succs)
  (branch-block terms succs))

(define (block-set/add-succ block succ)
  (cond [(basic-block? block) (basic-block-set-succ! block succ)]
        [else (branch-block-set-succs! block
                                       (set-add (branch-block-succs block)
                                                succ))]))

(define (block-succs block)
  (cond [(basic-block? block) (set (basic-block-succ block))]
        [else (branch-block-succs block)]))

(define (block-nodes block)
  (cond [(basic-block? block) (basic-block-nodes block)]
        [else (branch-block-nodes block)]))
