#lang racket
(provide make-node-graph node-graph-source

         node-graph-add-node node-graph-add-node/succs node-graph-get-node
         node-graph-add-succ node-graph-get-succs
         node-graph-set-source/key)

;; A [NodeGraph Node] is a (node-graph [Hash Key [SetOf Key]]
;;                                     [Hash Key Node]
;;                                     [SetOf Key]
;;                                     [Unit -> Key]
;;                                     [Unit -> Unit])

;; A Key is a NonNegativeInteger
;; A Key* is a [U Key [Box Key]]

(struct node-graph (graph uidmapping source next-key inc-key) #:transparent)

;; make-node-graph : Unit -> [NodeGraph Key Node]
;; this initializes the key counter and returns an empty node-graph
(define (make-node-graph)
  (node-graph (hash)
              (hash)
              (set)
              0
              add1))

;; node-graph-add-node : [NodeGraph Key Node]
;;                       Node
;;                       ->
;;                       (values [NodeGraph Key Node] Key)
(define (node-graph-add-node ng new-node)
  (match-define (node-graph graph uid-mapping source next-key inc-key) ng)
  (values (node-graph graph
                      (hash-set uid-mapping next-key new-node)
                      source
                      (inc-key next-key)
                      inc-key)
          next-key))

;; node-graph-set-source/key : [NodeGraph Key Node]
;;                             Key
;;                             ->
;;                             [NodeGraph Key Node]
(define (node-graph-set-source/key ng source-key)
  (match-define (node-graph graph uid-mapping source next-key inc-key) ng)
  (node-graph graph
              uid-mapping
              source-key
              next-key
              inc-key))

;; node-graph-add-node/succs : [NodeGraph Key Node]
;;                             Node
;;                             [Sequence Key]]
;;                             ->
;;                             [NodeGraph Key Node]
;;                             Key
(define (node-graph-add-node/succs ng new-node succs)
  (let-values (((ng* key) (node-graph-add-node ng new-node)))
    (values (for/fold ((ng* ng*))
                      ((succ succs))
              (node-graph-add-succ ng* key succ))
            key)))

;; node-graph-add-succ : [NodeGraph Key Node] Key Key -> [NodeGraph Key Node]
(define (node-graph-add-succ ng key succ-key)
  (match-define (node-graph graph uid-mapping source next-key inc-key) ng)
  (node-graph (hash-set graph
                        key
                        (set-add (hash-ref graph key (set))
                                 succ-key))
              uid-mapping source next-key inc-key))

;; node-graph-get-node : [NodeGraph Key Node] Key -> Node
(define (node-graph-get-node ng key)
  (hash-ref (node-graph-uidmapping ng)
            key
            (lambda ()
              (error 'node-graph
                     "The key, ~a, was not found in the given node graph"
                     key))))

;; node-graph-get-succ : [NodeGraph Key Node] Key -> [SetOf Key]
(define (node-graph-get-succs ng key #:flatten-boxes [flatten? #t])
  (let ((succs (hash-ref (node-graph-graph ng) key (set))))
    (if flatten? (flatten-boxes succs) succs)))

;; flatten-boxes : [Set [U X [Box X]]] -> [Set X]
(define (flatten-boxes s)
  (for/fold ((s (set)))
            ((e (in-set s)))
    (set-add s (if (box? e) (unbox e) e))))