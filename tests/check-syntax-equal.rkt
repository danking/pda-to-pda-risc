#lang racket
(require rackunit)
(provide check-syntax-equal?)

;; don't pass infinite sequences to this.
(define (syntax-equal? x y)
  (let-values (((x y) (if (and (syntax? x) (syntax? y))
                          (values (syntax->datum x) (syntax->datum y))
                          (values x y))))
    (let-values (((x y) (if (and (struct? x) (struct? y))
                            (values (struct->vector x) (struct->vector y))
                            (values x y))))
      (cond [(and (hash? x) (hash? y)
                  (= (sequence-length x)
                     (sequence-length y)))
             (for/and ([(x-key x-val) x])
               (and (hash-has-key? y x-key)
                    (syntax-equal? x-val (hash-ref y x-key))))]
            [(and (sequence? x) (sequence? y)
                  (= (sequence-length x)
                     (sequence-length y)))
             (for/and ([x-element x]
                       [y-element y])
                      (syntax-equal? x-element y-element))]
            [else (equal? x y)]))))

(define-binary-check (check-syntax-equal? syntax-equal? actual expected))

