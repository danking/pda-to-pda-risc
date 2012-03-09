#lang racket
(require rackunit)
(require/expose "../graph-algo.rkt"
                (infer-stack-types
                 truncate-to
                 join-stack-types
                 stack-type))

(define g (hash 0 (set '(3 a) '(6 c) '(1 a) '(4 b))
                1 (set '(2 b) '(5 d))
                2 (set '(3 c))
                3 (set)
                4 (set '(5 c))
                5 (set '(3 d))
                6 (set '(3 a))))

(define g-loop (hash 0 (set '(3 a) '(6 c) '(1 a) '(4 b))
                     1 (set '(2 b) '(5 d))
                     2 (set '(3 c))
                     3 (set '(3 z))
                     4 (set '(5 c))
                     5 (set '(3 d))
                     6 (set '(3 a))))

(define g-double-loop (hash 0 (set '(3 a) '(6 c) '(1 a) '(4 b))
                            1 (set '(2 b) '(5 d))
                            2 (set '(3 c) '(2 z))
                            3 (set '(3 z))
                            4 (set '(5 c))
                            5 (set '(3 d))
                            6 (set '(3 a))))

(define g-double-loop-no-0-3-edge
  (hash 0 (set '(6 c) '(1 a) '(4 b))
        1 (set '(2 b) '(5 d))
        2 (set '(3 c) '(2 z))
        3 (set '(3 z))
        4 (set '(5 c))
        5 (set '(3 d))
        6 (set '(3 a))))

(check-equal? (infer-stack-types g 0)
              (hash 0
                    (set '())
                    1
                    (set '(a 0))
                    2
                    (set '(b 1 a 0))
                    3
                    (set '(c 2) '(a 6) '(a 0) '(d 5))
                    4
                    (set '(b 0))
                    5
                    (set '(d 1 a 0) '(c 4 b 0))
                    6
                    (set '(c 0))))

(check-equal? (infer-stack-types g-loop 0)
              (hash 0
                    (set '())
                    1
                    (set '(a 0))
                    2
                    (set '(b 1 a 0))
                    3
                    (set '(c 2) '(a 6) '(a 0) '(z 3) '(d 5))
                    4
                    (set '(b 0))
                    5
                    (set '(d 1 a 0) '(c 4 b 0))
                    6
                    (set '(c 0))))

(check-equal? (infer-stack-types g-double-loop 0)
              (hash 0
                    (set '())
                    1
                    (set '(a 0))
                    2
                    (set '(b 1 a 0) '(z 2 b 1) '(z 2 z 2))
                    3
                    (set '(c 2) '(a 6) '(a 0) '(z 3) '(d 5))
                    4
                    (set '(b 0))
                    5
                    (set '(d 1 a 0) '(c 4 b 0))
                    6
                    (set '(c 0))))

(check-equal? (infer-stack-types g-double-loop-no-0-3-edge 0)
              (hash 0
                    (set '())
                    1
                    (set '(a 0))
                    2
                    (set '(b 1 a 0) '(z 2 b 1) '(z 2 z 2))
                    3
                    (set
                     '(d 5 c 4)
                     '(a 6 c 0)
                     '(c 2 b 1)
                     '(z 3 c 2)
                     '(z 3 z 3)
                     '(z 3 d 5)
                     '(c 2 z 2)
                     '(d 5 d 1)
                     '(z 3 a 6))
                    4
                    (set '(b 0))
                    5
                    (set '(d 1 a 0) '(c 4 b 0))
                    6
                    (set '(c 0))))

(check-equal? (truncate-to (stack-type (set '(C B A) '(D E F) '(B A C) '(A B C)) 3) 2)
              (stack-type (set '(C B) '(D E) '(B A) '(A B)) 2))
(check-equal? (truncate-to (stack-type (set '(D D) '(D C)) 2) 1)
              (stack-type (set '(D)) 1))

(check-equal? (join-stack-types (stack-type (set '(C B A) '(D E F) '(B A C) '(A B C)) 3)
                                (stack-type (set '(D E) '(F G) '(C B) '(A B) '(A C)) 2))
              (stack-type (set '(A C) '(F G) '(C B) '(D E) '(B A) '(A B)) 2))
(check-equal? (join-stack-types (stack-type (set '(D D)) 2)
                                (stack-type (set '(D C)) 2))
              (stack-type (set '(D C) '(D D)) 2))
(check-equal? (join-stack-types (stack-type (set '(D D) '(A B)) 2)
                                (stack-type (set '(B A) '(A B) '(D C)) 2))
              (stack-type (set '(D C) '(B A) '(D D) '(A B)) 2))
(check-equal? (join-stack-types (stack-type (set) 0)
                                (stack-type (set) 0))
              (stack-type (set) 0))
