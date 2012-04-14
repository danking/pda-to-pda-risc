#lang racket
(provide stack-type stack-type-stacks stack-type-length
         bottom bottom?
         transition join-stack-types)

(define-struct stack-type (stacks length) #:transparent)
(define-struct bottom-stack-type ())
(define bottom (bottom-stack-type))
(define bottom? bottom-stack-type?)

;; transition : StackType State Symbol -> StackType
;; computes the stack type if the given edge was traversed from the given
;; source node
(define (transition type source sym)
  (stack-type (for/set ([stack (in-set (stack-type-stacks type))])
                (list* sym source stack))
              (+ 2 (stack-type-length type))))

(define (join-stack-types type1 type2)
  (cond [(bottom? type1) type2]         ; #f is bottom
        [(bottom? type2) type1]
        [else (let-values (((type1 type2) (equate-lengths type1 type2)))
                (make-stack-type (set-union (stack-type-stacks type1)
                                            (stack-type-stacks type2))
                                 (stack-type-length type1)))]))

(define (equate-lengths type1 type2)
  (let ((l1 (stack-type-length type1))
        (l2 (stack-type-length type2)))
    (cond [(> l1 l2) (values (truncate-to type1 l2) type2)]
          [(> l2 l1) (values type1 (truncate-to type2 l1))]
          [else (values type1 type2)])))

(define (truncate-to type n)
  (stack-type (for/set ([stack (in-set (stack-type-stacks type))])
                (let loop ((n n)
                           (acc empty)
                           (stack stack))
                  (if (zero? n)
                      (reverse acc)
                      (loop (sub1 n)
                            (cons (first stack) acc)
                            (rest stack)))))
              n))
