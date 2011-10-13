#lang racket
(require rackunit
         srfi/1)
(provide (all-defined-out))
#|

while (not (empty? work)) and?? (type is unchanged)
  (from, to, tok) = work

  new-stack-types = tok :: form :: type [from]

  if uninitialized?( type[to] )
    type[to] = new-stack-types
  else
    n = min( length(first(type[to])), length(first(new-stack-types)))
    new-stack-types = truncate-to (new-stack-types, n)
    type[to] = truncate-to (type[to], n)
    type [to] = Union (new-stack-types, type [to])

|#


(define g '#(((3 a) (6 c) (1 a) (4 b))
             ((2 b) (5 d))
             ((3 c))
             ()
             ((5 c))
             ((3 d))
             ((3 a))))

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
         (loop (append more-work (aug-edges g dest))
               (if (false? dest-stack-types)
                   (dict-set types dest new-stack-types)
                   (dict-set types
                             dest
                             (union-stack-sets dest-stack-types
                                               new-stack-types)))))))))

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

(check-equal? (truncate-to '((C B A) (D E F) (B A C) (A B C)) 2)
              '((C B) (D E) (B A) (A B)))
(check-equal? (truncate-to '((D D) (D C)) 1)
              '((D) (D)))

(check-equal? (union-stack-sets '((C B A) (D E F) (B A C) (A B C))
                                '((D E) (F G) (C B) (A B) (A C)))
              '((A C) (F G) (C B) (D E) (B A) (A B)))
(check-equal? (union-stack-sets '((D D)) '((D C)))
              '((D C) (D D)))
(check-equal? (union-stack-sets '((D D) (A B)) '((B A) (A B) (D C)))
              '((D C) (B A) (D D) (A B)))
(check-equal? (union-stack-sets '(()) '(())) '(()))
