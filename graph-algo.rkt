#lang racket
(require srfi/1)
(provide assign-types)

;; Graph Number -> [ListOf [Pair Number Symbol]]
(define (edges g s) (vector-ref g s))

;; Graph Number -> [ListOf (list Number Number Symbol)]
(define (aug-edges g s)
  (map (curry cons s) (edges g s)))

;; add-trans : Number Symbol StackType -> StackType
;; adds a note of the transition to the stack type
(define (add-trans source token type)
  (map (curry list* token source) type))

;; a Graph is a [Vector [ListOf [Pair Number Symbol]]]
;; where the pairs are edges of the form (destination shift-token) and the
;; source node is the vector index

;; assign-types : Graph -> [Dict Number StackType]
(define (assign-types g s)
  (let loop ((work (aug-edges g s))
             (types (hasheq s '(()))))
    (match work
      (`() types)
      (`((,source ,dest ,token) ,more-work ...)
       (let ((new-stack-types (add-trans source
                                         token
                                         (dict-ref types
                                                   source
                                                   (unvisited-err source
                                                                  dest
                                                                  token))))
             (dest-stack-types (dict-ref types
                                         dest
                                         #f)))
         (if (false? dest-stack-types)
             (loop (append more-work (aug-edges g dest))
                   (dict-set types dest new-stack-types))
             (let ((union-of-types (union-stack-sets dest-stack-types
                                                     new-stack-types)))
               (loop (if (equal? union-of-types dest-stack-types)
                         more-work
                         (append more-work (aug-edges g dest)))
                     (dict-set types
                               dest
                               union-of-types)))))))))

(define (unvisited-err source dest token)
  (lambda ()
    (error 't "must visit the source node first! (~a ~a ~a)"
           source dest token)))


;; A Stack is either
;;  - empty
;;  - (cons Symbol Stack)

;; union-stack-sets : [ListOf Stack] [ListOf Stack] -> [ListOf Stack]
;; computes the union of two sets of stacks
(define (union-stack-sets s1 s2)
  (let-values (((s1 s2)
                (match-lengths-in-sets s1 s2)))
    (lset-union equal? s1 s2)))

(define (match-lengths-in-sets s1 s2)
  (let ((l1 (length (first s1)))
        (l2 (length (first s2))))
    (cond [(< l1 l2) (values s1 (truncate-to s2 l1))]
          [(> l1 l2) (values (truncate-to s1 l2) s2)]
          [else (values s1 s2)])))

;; truncate-to : [ListOf Stack] Number -> [ListOf Stack]
(define (truncate-to los n)
  (let loop ((los los)
             (n n)
             (acc (map (lambda (x) '()) los)))
    (if (zero? n)
        (map reverse acc)
        (loop (map rest los)
              (sub1 n)
              (map cons
                   (map first los)
                   acc)))))
