#lang racket
(provide check-syntax-equal?
         syntax-equal?
         vs->ls)

(define-syntax vs->ls
  (syntax-rules ()
    ((_ exp)
     (call-with-values (lambda () exp) list))))

(require rackunit)

;; don't pass infinite sequences to this.
(define (syntax-equal? x y)
  (let-values (((x y) (if (and (syntax? x) (syntax? y))
                          (values (syntax->datum x) (syntax->datum y))
                          (values x y))))
    (let-values (((x y) (if (and (struct? x) (struct? y))
                            (values (struct->vector x) (struct->vector y))
                            (values x y))))
      (cond [(and (dict? x) (dict? y)
                  (not (list? x))
                  (not (list? y))
                  (= (sequence-length (in-dict x))
                     (sequence-length (in-dict y))))
             (for/and ([(x-key x-val) (in-dict x)])
                      (let ((y-key (findf (lambda (k)
                                            (syntax-equal? x-key k))
                                          (dict-keys y))))
                        (and y-key
                             (syntax-equal? x-val
                                            (dict-ref y y-key)))))]
            [(and (sequence? x) (sequence? y)
                  (not (number? x))
                  (not (number? y))
                  (= (sequence-length x)
                     (sequence-length y)))
             (for/and ([x-element x]
                       [y-element y])
                      (syntax-equal? x-element y-element))]
            [else (equal? x y)]))))

(define-binary-check (check-syntax-equal? syntax-equal? actual expected))
