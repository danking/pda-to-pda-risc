#lang racket
(require "../pdarisc-data.rkt"
         rackunit
         "test-util.rkt")
(require/expose "../compile-pdarisc.rkt"
                (compile-pdarisc
                 compile-pure-rhs
                 compile-insn-seq*))


(define (compile-insn-seq*/default insn-seq*)
  (compile-insn-seq* insn-seq* #'(lambda (x) x) #'car #'cdr #'empty?))

;; Pure-RHS
(check-syntax-equal? (compile-pure-rhs (make-state 's1))
                     #'(state 's1))
(check-syntax-equal? (compile-pure-rhs (make-nterm 'foo))
                     #'(nterm 'foo))
(check-syntax-equal? (compile-pure-rhs (make-curr-token #f))
                     #'TR)
(check-syntax-equal? (compile-pure-rhs (make-named-reg 'some-var))
                     #'r-some-var)
(check-syntax-equal? (compile-pure-rhs (make-nameless-reg))
                     #'__)

;; Insn-Seq*
(define dummy-accept
  (make-accept 1 (list (make-named-reg 'dummy))))
(define dummy-accept/racket
  #'(values r-dummy))

(check-syntax-equal? (compile-insn-seq*/default (list dummy-accept))
                     dummy-accept/racket)

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-assign 2 (make-named-reg 'v) (make-pop))
                            dummy-accept))
                     #`(let ((r-v (begin0 (car stk) (set! stk (cdr stk)))))
                         #,dummy-accept/racket))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-assign 2
                                         (make-named-reg 'v)
                                         (make-curr-token #f))
                            dummy-accept))
                     #`(let ((r-v TR))
                         #,dummy-accept/racket))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-push 2 (make-curr-token #f))
                            dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket in
                                                tr
                                                regs
                                                (cons tr stack))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-push 2 (make-state 's0))
                            dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket in
                                                tr
                                                regs
                                                (cons (make-state 's0) stack))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-push 2 (make-named-reg 'foo))
                            dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket in
                                                tr
                                                regs
                                                (cons (dict-ref regs 'foo)
                                                      stack))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-sem-act 2
                                          #'r1
                                          (list (make-named-reg 'v1)
                                                (make-named-reg 'v2))
                                          (list (make-named-reg 'ret-val))
                                          #'(+ v1 v2))
                            dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket in
                                                tr
                                                (dict-set regs
                                                          'ret-val
                                                          ((lambda (v1 v2)
                                                             (+ v1 v2))
                                                           (dict-ref regs 'v1)
                                                           (dict-ref regs 'v2)))
                                                stack)))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-drop-token 2) dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket in #f regs stack)))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-get-token 2) dummy-accept))
                     #`(lambda (in tr regs stack)
                         (#,dummy-accept/racket (cdr in) (car in) regs stack)))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-block 4
                             (list
                              (make-get-token 2)
                              (make-drop-token 3)))
                            (make-go 5
                                     (make-label-name 's0)
                                     (list (make-named-reg 'x)
                                           (make-named-reg 'y)
                                           (make-named-reg 'z)))))
                     #`(lambda (in tr regs stack)
                         ((lambda (in tr regs stack)
                            ((lambda (in tr regs stack)
                               ((s0 (dict-ref regs 'x)
                                    (dict-ref regs 'y)
                                    (dict-ref regs 'z)) in tr regs stack))
                             in #f regs stack))
                          (cdr in) (car in) regs stack)))

;; this test must be verified by hand because x and y are gensym'ed
#;
(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-label (list (make-label-name 's0))
                                        '((())) ; stacks
                                        '(#f) ; token-regs
                                        (list (list (make-named-reg 'x)
                                                    (make-named-reg 'y)))
                                        (list
                                         (list dummy-accept))
                                        (list dummy-accept))))
                     #`(lambda (in tr regs stack)
                         (letrec ((s0 (lambda (x y) ;; x and y are gensym'd
                                        (lambda (in tr regs stack)
                                          (#,dummy-accept/racket in
                                                                 tr
                                                                 (dict-set
                                                                  (dict-set regs
                                                                            'x
                                                                            x)
                                                                  'y
                                                                  y)
                                                                 stack)))))
                           (lambda (in tr regs stack)
                             (#,dummy-accept/racket in
                                                    tr
                                                    regs
                                                    stack)))))

(check-syntax-equal? (compile-insn-seq*/default (list (make-block* 3
                                                       (list
                                                        (make-get-token 2)
                                                        (make-go 4
                                                         (make-label-name 's0)
                                                         '())))))
                     #`(lambda (in tr regs stack)
                         ((lambda (in tr regs stack)
                            ((s0) in tr regs stack))
                          (cdr in) (car in) regs stack)))

(check-syntax-equal? (compile-insn-seq*/default (list dummy-accept))
                     dummy-accept/racket)

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-if-eos 2 dummy-accept dummy-accept)))
                     #`(lambda (in tr regs stack)
                         (if (empty? in)
                             (#,dummy-accept/racket in tr regs stack)
                             (#,dummy-accept/racket in tr regs stack))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-state-case 4
                                             (make-named-reg 'target)
                                             (list (make-state #'s1)
                                                   (make-state #'s2))
                                             (list
                                              (list
                                               (make-go 2
                                                (make-label-name 's2)
                                                '()))
                                              (list
                                               (make-go 3
                                                (make-label-name 's0)
                                                '()))))))
                     #`(lambda (in tr regs stack)
                         (case (state-id (dict-ref regs 'target))
                           ((s1) ((lambda (in tr regs stack)
                                    ((s2) in tr regs stack))
                                  in tr regs stack))
                           ((s2) ((lambda (in tr regs stack)
                                    ((s0) in tr regs stack))
                                  in tr regs stack)))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-token-case 4
                             (list #'A #'B)
                             (list
                              (list (make-go 2
                                             (make-label-name 's1)
                                             '()))
                              (list (make-go 3
                                             (make-label-name 'r1)
                                             '()))))))
                     #`(lambda (in tr regs stack)
                         (case ((lambda (x) x) tr)
                           ((A) ((lambda (in tr regs stack)
                                   ((s1) in tr regs stack))
                                 in tr regs stack))
                           ((B) ((lambda (in tr regs stack)
                                   ((r1) in tr regs stack))
                                 in tr regs stack)))))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-go 2 (make-label-name 's0) '())))
                     #`(lambda (in tr regs stack)
                         ((s0) in tr regs stack)))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-go 2
                                     (make-label-name 's0)
                                     (list (make-named-reg 'x)
                                           (make-named-reg 'y)
                                           (make-named-reg 'z)))))
                     #`(lambda (in tr regs stack)
                         ((s0 (dict-ref regs 'x)
                              (dict-ref regs 'y)
                              (dict-ref regs 'z))
                          in tr regs stack)))

(check-syntax-equal? (compile-insn-seq*/default
                      (list (make-go 2
                                     (make-label-name 's0)
                                     (list (make-named-reg 'x)
                                           (make-named-reg 'y)
                                           (make-named-reg 'z)))))
                     #`(lambda (in tr regs stack)
                         ((s0 (dict-ref regs 'x)
                              (dict-ref regs 'y)
                              (dict-ref regs 'z))
                          in tr regs stack)))

(define s0
  (list (make-block* 3
         (list
          (make-if-eos 7
           (make-go 5 (make-label-name 's0-eos) '())
           (make-block* 4
            (list
             (make-get-token 2)
             (make-go 6
                      (make-label-name 's0-have-token)
                      '()))))))))
(define s0/racket
  #'(lambda (in tr regs stack)
      (if (empty? in)
          ((lambda (in tr regs stack)
             ((s0-eos) in tr regs stack))
           in tr regs stack)
          ((lambda (in tr regs stack)
             ((lambda (in tr regs stack)
                ((s0-have-token) in tr regs stack))
              (cdr in) (car in) regs stack))
           in tr regs stack))))

(check-syntax-equal? (compile-insn-seq*/default s0) s0/racket)

(define s1
  (list (make-block* 3
         (list
          (make-if-eos 7
           (make-go 5 (make-label-name 's1-eos) '())
           (make-block* 4
            (list
             (make-get-token 2)
             (make-go 6
                      (make-label-name 's1-have-token)
                      '()))))))))

(define s1/racket
  #'(lambda (in tr regs stack)
      (if (empty? in)
          ((lambda (in tr regs stack)
             ((s1-eos) in tr regs stack))
           in tr regs stack)
          ((lambda (in tr regs stack)
             ((lambda (in tr regs stack)
                ((s1-have-token) in tr regs stack))
              (cdr in) (car in) regs stack))
           in tr regs stack))))

(check-syntax-equal? (compile-insn-seq*/default s1) s1/racket)

(define s0-have-token
  (list (make-block* 5
         (list (make-token-case 7
                (list #'A #'B)
                (list
                 (list (make-push 2 (make-state #'s0))
                       (make-push 3 (make-curr-token #f))
                       (make-drop-token 4)
                       (make-go 5
                                (make-label-name 's1)
                                '()))
                 (list (make-go 6
                                (make-label-name 'r1-have-token)
                                '()))))))))
(define s0-have-token/racket
  #'(lambda (in tr regs stack)
      (case ((lambda (x) x) tr)
        ((A) ((lambda (in tr regs stack)
                ((lambda (in tr regs stack)
                   ((lambda (in tr regs stack)
                      ((lambda (in tr regs stack)
                         ((s1) in tr regs stack))
                       in #f regs stack))
                    in tr regs (cons tr stack)))
                 in tr regs (cons (make-state 's0) stack)))
              in tr regs stack))
        ((B) ((lambda (in tr regs stack)
                ((r1-have-token) in tr regs stack))
              in tr regs stack)))))

(check-syntax-equal? (compile-insn-seq*/default s0-have-token)
                     s0-have-token/racket)

(define s1-have-token
  (list (make-block* 5
         (list (make-token-case 7
                (list #'A #f)
                (list
                 (list (make-push 2 (make-state #'s1))
                       (make-push 3 (make-curr-token #f))
                       (make-drop-token 4)
                       (make-go 5
                                (make-label-name 's1)
                                '()))
                 (list (make-go 6
                                (make-label-name 'r1-have-token)
                                '()))))))))

(define s1-have-token/racket
  #'(lambda (in tr regs stack)
      (case ((lambda (x) x) tr)
        ((A) ((lambda (in tr regs stack)
                ((lambda (in tr regs stack)
                   ((lambda (in tr regs stack)
                      ((lambda (in tr regs stack)
                         ((s1) in tr regs stack))
                       in #f regs stack))
                    in tr regs (cons tr stack)))
                 in tr regs (cons (make-state 's1) stack)))
              in tr regs stack))
        (else ((lambda (in tr regs stack)
                 ((r1-have-token) in tr regs stack))
               in tr regs stack)))))

(check-syntax-equal? (compile-insn-seq*/default s1-have-token)
                     s1-have-token/racket)

(define s0-eos
  (list (make-block* 1
         (list (make-accept 2 (list (make-named-reg 'reject)))))))

(define s0-eos/racket
  #'(lambda (in tr regs stack)
      (list (dict-ref regs 'reject))))

(check-syntax-equal? (compile-insn-seq*/default s0-eos) s0-eos/racket)

(define s1-eos
  (list (make-block* 1
         (list (make-go 2
                        (make-label-name 'r1-eos)
                        '())))))

(define s1-eos/racket
  #'(lambda (in tr regs stack)
      ((r1-eos) in tr regs stack)))

(check-syntax-equal? (compile-insn-seq*/default s1-eos) s1-eos/racket)

(define r1
  (list (make-block* 7
         (list
          (make-assign 6 (make-named-reg 'v) (make-pop))
          (make-assign 5 (make-named-reg 'target) (make-pop)) ; state
          (make-sem-act 4
                        #'r1
                        (list (make-named-reg 'v))
                        (list (make-named-reg 'ret-val))
                        #'v)
          (make-push 2 (make-named-reg 'target))
          (make-push 3 (make-named-reg 'ret-val))
          (make-state-case 8
                           (make-named-reg 'target)
                           (list (make-state #'s0)
                                 (make-state #'s1))
                           (list
                            (list
                             (make-go 9
                              (make-label-name 's1-have-token)
                              '()))
                            (list
                             (make-go 10
                              (make-label-name 's0-have-token)
                              '()))))))))

(define r1/racket
  #'(lambda (in tr regs stack)
      ((lambda (in tr regs stack)
         ((lambda (in tr regs stack)
            ((lambda (in tr regs stack)
               ((lambda (in tr regs stack)
                  ((lambda (in tr regs stack)
                     (case (state-id (dict-ref regs 'target))
                       ((s0) ((lambda (in tr regs stack)
                                ((s1-have-token) in tr regs stack))
                              in tr regs stack))
                       ((s1) ((lambda (in tr regs stack)
                                ((s0-have-token) in tr regs stack))
                              in tr regs stack))))
                   in tr regs (cons (dict-ref regs 'ret-val) stack)))
                in tr regs (cons (dict-ref regs 'target) stack)))
             in
             tr
             (dict-set regs 'ret-val
                       ((lambda (v) v) (dict-ref regs 'v)))
             stack))
          in tr (dict-set regs 'target (car stack)) (cdr stack)))
       in tr (dict-set regs 'v (car stack)) (cdr stack))))

(check-syntax-equal? (compile-insn-seq*/default r1) r1/racket)

(define r1-eos
  (list (make-block* 7
         (list
          (make-assign 6 (make-named-reg 'v) (make-pop))
          (make-assign 5 (make-named-reg 'target) (make-pop)) ; state
          (make-sem-act 4
                        #'r1
                        (list (make-named-reg 'v))
                        (list (make-named-reg 'ret-val))
                        #'v)
          (make-push 2 (make-named-reg 'target))
          (make-push 3 (make-named-reg 'ret-val))
          (make-state-case 8
                           (make-named-reg 'target)
                           (list (make-state #'s0)
                                 (make-state #'s1))
                           (list
                            (list
                             (make-go 9
                              (make-label-name 's1-eos)
                              '()))
                            (list
                             (make-go 10
                              (make-label-name 's0-eos)
                              '()))))))))

(define r1-eos/racket
  #'(lambda (in tr regs stack)
      ((lambda (in tr regs stack)
         ((lambda (in tr regs stack)
            ((lambda (in tr regs stack)
               ((lambda (in tr regs stack)
                  ((lambda (in tr regs stack)
                     (case (state-id (dict-ref regs 'target))
                       ((s0) ((lambda (in tr regs stack)
                                ((s1-eos) in tr regs stack))
                              in tr regs stack))
                       ((s1) ((lambda (in tr regs stack)
                                ((s0-eos) in tr regs stack))
                              in tr regs stack))))
                   in tr regs (cons (dict-ref regs 'ret-val) stack)))
                in tr regs (cons (dict-ref regs 'target) stack)))
             in
             tr
             (dict-set regs 'ret-val
                       ((lambda (v) v) (dict-ref regs 'v)))
             stack))
          in tr (dict-set regs 'target (car stack)) (cdr stack)))
       in tr (dict-set regs 'v (car stack)) (cdr stack))))

(check-syntax-equal? (compile-insn-seq*/default r1-eos) r1-eos/racket)

;; compile-pdarisc
(check-syntax-equal? (compile-pdarisc
                      (make-pdarisc 3
                       (list (make-label 2
                              (list (make-label-name 's0)
                                    (make-label-name 's1)
                                    (make-label-name 's0-have-token)
                                    (make-label-name 's1-have-token)
                                    (make-label-name 's0-eos)
                                    (make-label-name 's1-eos)
                                    (make-label-name 'r1)
                                    (make-label-name 'r1-eos))
                              '((()) (()) (()) (()) (()) (())
                                (s : ((A s))) (s : ((A s)))) ; stacks
                              '(#f #f #f #f #f #f #f #f) ; token-regs
                              '(() () () () () () () ()) ; arg-lists
                              (list s0
                                    s1
                                    s0-have-token
                                    s1-have-token
                                    s0-eos
                                    s1-eos
                                    r1
                                    r1-eos)
                              (list (make-go 3 (make-label-name 's0) '())))))
                      #'(lambda (x) x)
                      #'car
                      #'cdr
                      #'empty?)
                     #`(lambda (input-stream)
                         (define-struct nterm (id) #:transparent)
                         (define-struct state (id) #:transparent)
                         ((lambda (in tr regs stack)
                            (letrec ((s0 (lambda ()
                                           (lambda (in tr regs stack)
                                             (#,s0/racket in tr regs stack))))
                                     (s1 (lambda ()
                                           (lambda (in tr regs stack)
                                             (#,s1/racket in tr regs stack))))
                                     (s0-have-token
                                      (lambda ()
                                        (lambda (in tr regs stack)
                                          (#,s0-have-token/racket in
                                                                  tr
                                                                  regs
                                                                  stack))))
                                     (s1-have-token
                                      (lambda ()
                                        (lambda (in tr regs stack)
                                          (#,s1-have-token/racket in
                                                                  tr
                                                                  regs
                                                                  stack))))
                                     (s0-eos (lambda ()
                                               (lambda (in tr regs stack)
                                                 (#,s0-eos/racket in
                                                                  tr
                                                                  regs
                                                                  stack))))
                                     (s1-eos (lambda ()
                                               (lambda (in tr regs stack)
                                                 (#,s1-eos/racket in
                                                                  tr
                                                                  regs
                                                                  stack))))
                                     (r1 (lambda ()
                                           (lambda (in tr regs stack)
                                             (#,r1/racket in tr regs stack))))
                                     (r1-eos (lambda ()
                                               (lambda (in tr regs stack)
                                                 (#,r1-eos/racket in
                                                                  tr
                                                                  regs
                                                                  stack)))))
                              ((lambda (in tr regs stack)
                                 ((s0) in tr regs stack))
                               in tr regs stack)))
                          input-stream
                          #f
                          (hasheq)
                          '())))
