#lang racket
(require "stack-types.rkt")
(provide infer-stack-types)

;; edges : States -> P(States × Symbol)
;; type : States -> StackType

;; StackType = P(Stacks) ∪ False
;; A StackType is a set of stacks that are valid at a given state
;; False means we know nothing about the stack type (bottom)

;; Stacks-0 = ∅
;; Stacks-n = Symbol × States × Stack-n-1
;; Stacks = ⋃ Stack-i

(require "../misc/set-utilities.rkt")

;; A [StackType T] is a (stack-type [SetOf [Stack T]] NaturalNumber)

;; A [Stack T] is either
;;  - empty, or
;;  - (cons Symbol (cons T [StackType T]))

;; infer-stack-types : [Hash T [Set T]] -> [Hash T [ListOf [Stack T]]]
;; this infers all the possible paths through the given graph (recording both
;; node and edge symbol) thus producing the "stack type" at every given node
(define (infer-stack-types edges source-node)
  (define (loop workset types)
    (if (set-empty? workset)
        (for/hash (((k e) types))
          (values k
                  (for/list ((stack (in-set (stack-type-stacks e))))
                    stack)))
        (let-values (((source workset) (set-get-one/rest workset)))
          (let-values
              (((workset* types)
                (for/fold ([workset workset]
                           [types types])
                    ([edge (hash-ref edges source (err-no-edge source))])
                  (let ((dest (first edge))
                        (sym  (second edge)))
                    (let* ((new-stack-type (transition (hash-ref types
                                                                 source
                                                                 (err-no-type source))
                                                       source
                                                       sym))
                           (old-stack-type (hash-ref types dest bottom))
                           (joined-stack-type (join-stack-types new-stack-type
                                                                old-stack-type)))
                      (if (equal? joined-stack-type old-stack-type)
                          (values workset types)
                          (values (set-add workset dest)
                                  (hash-set types dest joined-stack-type))))))))
            (loop workset* types)))))
  (loop (set source-node) (hash source-node (stack-type (set '()) 0))))

(define (err-no-edge node)
  (lambda ()
    (error 'err-no-edge
           "The given node ~a doesn't have any edges, is it in the graph?"
           node)))

(define (err-no-type node)
  (lambda ()
    (error 'err-no-edge
           (string-append "The given node ~a doesn't have a type; therefore, it "
                          "hasn't been visited, but we're still trying to "
                          "transition from it?")
           node)))
