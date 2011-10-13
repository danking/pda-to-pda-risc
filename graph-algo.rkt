#lang racket
(require rackunit
         srfi/1)
#|

while (not (empty? work)) and?? (type is unchanged)
  (from, to, tok) = work
  if type [to] = empty
    type [to] = tok :: from :: type [from]
  else
    n = minlength (type [to])
    new-stack-types = truncate-to (tok :: form :: type [from], n)
    type [to] = Union (t, type [to])

|#


(define g '#(((4 a) (7 c) (2 a) (5 b))
             ((3 b) (6 d))
             ((4 c))
             ()
             ((6 c))
             ((4 d))
             ((4 a))))

;; Graph Number -> [ListOf [Pair Number Symbol]]
(define (edges g s) (vector-ref g s))

;; Graph Number -> [ListOf (list Number Number Symbol)]
(define (aug-edges g s)
  (map (curry cons s) (edges g s)))

;; a Graph is a [Vector [ListOf [Pair Number Symbol]]]
;; where the pairs are edges of the form (destination shift-token) and the
;; source node is the vector index

;; assign-types : Graph -> [Dict Number StackType]
(define (assign-types g s)
  (let loop ((work (aug-edges ))
             (types (hasheq)))
    (if (empty? work)
        types
        (match ))))





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
