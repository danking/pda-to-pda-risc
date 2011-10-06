#lang racket
(require "../pdarisc-data.rkt"
         rackunit)
(require/expose "../compile-pdarisc.rkt"
                (compile-pdarisc
                 compile-pure-rhs
                 compile-insn-seq*))

;; Pure-RHS
(check-equal? (compile-pure-rhs (make-state 's1) 'tr 'regs)
              '(make-state 's1))
(check-equal? (compile-pure-rhs (make-nterm 'foo) 'tr 'regs)
              '(make-nterm 'foo))
(check-equal? (compile-pure-rhs (make-curr-token #f) 'tr 'regs)
              'tr)
(check-equal? (compile-pure-rhs (make-named-reg 'some-var) 'tr 'regs)
              '(dict-ref regs 'some-var))
(check-equal? (compile-pure-rhs (make-nameless-reg) 'tr 'regs)
              '(dict-ref regs '<nameless-register>))

;; Insn-Seq*
(define dummy-accept
  (make-accept (list (make-named-reg 'dummy))))
(define dummy-accept/racket
  '(lambda (in tr regs stack)
     (list (dict-ref regs 'dummy))))

(check-equal? (compile-insn-seq* (list dummy-accept)) dummy-accept/racket)

(check-equal? (compile-insn-seq*
               (list (make-assign (make-named-reg 'v) (make-pop))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       (dict-set regs 'v (car stack))
                                       (cdr stack))))

(check-equal? (compile-insn-seq*
               (list (make-assign (make-named-reg 'v) (make-curr-token #f))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       (dict-set regs 'v tr)
                                       stack)))

(check-equal? (compile-insn-seq*
               (list (make-push (make-curr-token #f))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       regs
                                       (cons tr stack))))

(check-equal? (compile-insn-seq*
               (list (make-push (make-state 's0))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       regs
                                       (cons (make-state 's0) stack))))

(check-equal? (compile-insn-seq*
               (list (make-push (make-named-reg 'foo))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       regs
                                       (cons (dict-ref regs 'foo) stack))))

(check-equal? (compile-insn-seq*
               (list (make-sem-act 'r1
                                   (list (make-named-reg 'v1)
                                         (make-named-reg 'v2))
                                   (list (make-named-reg 'ret-val))
                                   '(+ v1 v2))
                     dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in
                                       tr
                                       (dict-set regs
                                                 'ret-val
                                                 ((lambda (v1 v2)
                                                    (+ v1 v2))
                                                  (dict-ref regs 'v1)
                                                  (dict-ref regs 'v2)))
                                       stack)))

(check-equal? (compile-insn-seq*
               (list (make-drop-token) dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket in #f regs stack)))

(check-equal? (compile-insn-seq*
               (list (make-get-token) dummy-accept))
              `(lambda (in tr regs stack)
                 (,dummy-accept/racket (cdr in) (car in) regs stack)))

(check-equal? (compile-insn-seq* (list (make-block
                                        (list
                                         (make-get-token)
                                         (make-drop-token)))
                                       (make-go (make-label-name 's0)
                                                (list (make-named-reg 'x)
                                                      (make-named-reg 'y)
                                                      (make-named-reg 'z)))))
              `(lambda (in tr regs stack)
                 ((lambda (in tr regs stack)
                    ((lambda (in tr regs stack)
                       ((s0 (dict-ref regs 'x)
                            (dict-ref regs 'y)
                            (dict-ref regs 'z)) in tr regs stack))
                     in #f regs stack))
                  (cdr in) (car in) regs stack)))

;; this test must be verified by hand because x and y are gensym'ed
#;
(check-equal? (compile-insn-seq*
               (list (make-label (list (make-label-name 's0))
                                 '((())) ; stacks
                                 '(#f) ; token-regs
                                 (list (list (make-named-reg 'x)
                                             (make-named-reg 'y))) ; arg-lists
                                 (list
                                  (list dummy-accept))
                                 (list dummy-accept))))
              `(lambda (in tr regs stack)
                 (letrec ((s0 (lambda (x y) ;; x and y are gensym'd
                                (lambda (in tr regs stack)
                                  (,dummy-accept/racket in
                                                        tr
                                                        (dict-set (dict-set regs
                                                                            'x
                                                                            x)
                                                                  'y
                                                                  y)
                                                        stack)))))
                   (lambda (in tr regs stack)
                     (,dummy-accept/racket in
                                           tr
                                           regs
                                           stack)))))

(check-equal? (compile-insn-seq* (list (make-block*
                                        (list
                                         (make-get-token)
                                         (make-go (make-label-name 's0)
                                                  '())))))
              `(lambda (in tr regs stack)
                 ((lambda (in tr regs stack)
                    ((s0) in tr regs stack))
                  (cdr in) (car in) regs stack)))

(check-equal? (compile-insn-seq* (list dummy-accept)) dummy-accept/racket)

(check-equal? (compile-insn-seq*
               (list (make-if-eos dummy-accept dummy-accept)))
              `(lambda (in tr regs stack)
                 (if (empty? in)
                     (,dummy-accept/racket in tr regs stack)
                     (,dummy-accept/racket in tr regs stack))))

(check-equal? (compile-insn-seq*
               (list (make-state-case (make-named-reg 'target)
                                      (list (make-state 's1)
                                            (make-state 's2))
                                      (list
                                       (list
                                        (make-go
                                         (make-label-name 's2)
                                         '()))
                                       (list
                                        (make-go
                                         (make-label-name 's0)
                                         '()))))))
              `(lambda (in tr regs stack)
                 (case (state-id (dict-ref regs 'target))
                   ((s1) ((lambda (in tr regs stack)
                            ((s2) in tr regs stack))
                          in tr regs stack))
                   ((s2) ((lambda (in tr regs stack)
                            ((s0) in tr regs stack))
                          in tr regs stack)))))

(check-equal? (compile-insn-seq*
               (list (make-token-case
                      '(A B)
                      (list
                       (list (make-go (make-label-name 's1)
                                      '()))
                       (list (make-go (make-label-name 'r1)
                                      '()))))))
              `(lambda (in tr regs stack)
                 (case tr
                   ((A) ((lambda (in tr regs stack)
                           ((s1) in tr regs stack))
                         in tr regs stack))
                   ((B) ((lambda (in tr regs stack)
                           ((r1) in tr regs stack))
                         in tr regs stack)))))

(check-equal? (compile-insn-seq*
               (list (make-go (make-label-name 's0) '())))
              `(lambda (in tr regs stack)
                 ((s0) in tr regs stack)))

(check-equal? (compile-insn-seq*
               (list (make-go (make-label-name 's0) (list (make-named-reg 'x)
                                                          (make-named-reg 'y)
                                                          (make-named-reg 'z)))))
              `(lambda (in tr regs stack)
                 ((s0 (dict-ref regs 'x)
                      (dict-ref regs 'y)
                      (dict-ref regs 'z))
                  in tr regs stack)))

(check-equal? (compile-insn-seq*
               (list (make-go (make-label-name 's0) (list (make-named-reg 'x)
                                                          (make-named-reg 'y)
                                                          (make-named-reg 'z)))))
              `(lambda (in tr regs stack)
                 ((s0 (dict-ref regs 'x)
                      (dict-ref regs 'y)
                      (dict-ref regs 'z))
                  in tr regs stack)))

(define s0
  (list (make-block*
         (list
          (make-if-eos
           (make-go (make-label-polynym 's0 'eos) '())
           (make-block*
            (list
             (make-get-token)
             (make-go (make-label-polynym 's0 'have-token)
                      '()))))))))
(define s0/racket
  '(lambda (in tr regs stack)
     (if (empty? in)
         ((lambda (in tr regs stack)
            ((s0-eos) in tr regs stack))
          in tr regs stack)
         ((lambda (in tr regs stack)
            ((lambda (in tr regs stack)
               ((s0-have-token) in tr regs stack))
             (cdr in) (car in) regs stack))
          in tr regs stack))))

(check-equal? (compile-insn-seq* s0) s0/racket)

(define s1
  (list (make-block*
         (list
          (make-if-eos
           (make-go (make-label-polynym 's1 'eos) '())
           (make-block*
            (list
             (make-get-token)
             (make-go (make-label-polynym 's1 'have-token)
                      '()))))))))

(define s1/racket
  '(lambda (in tr regs stack)
     (if (empty? in)
         ((lambda (in tr regs stack)
            ((s1-eos) in tr regs stack))
          in tr regs stack)
         ((lambda (in tr regs stack)
            ((lambda (in tr regs stack)
               ((s1-have-token) in tr regs stack))
             (cdr in) (car in) regs stack))
          in tr regs stack))))

(check-equal? (compile-insn-seq* s1) s1/racket)

(define s0-have-token
  (list (make-block*
         (list (make-token-case
                '(A B)
                (list
                 (list (make-push (make-state 's0))
                       (make-push (make-curr-token #f))
                       (make-drop-token)
                       (make-go (make-label-name 's1)
                                '()))
                 (list (make-go (make-label-polynym 'r1
                                                    'have-token)
                                '()))))))))
(define s0-have-token/racket
  '(lambda (in tr regs stack)
     (case tr
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

(check-equal? (compile-insn-seq* s0-have-token) s0-have-token/racket)

(define s1-have-token
  (list (make-block*
         (list (make-token-case
                '(A #f)
                (list
                 (list (make-push (make-state 's1))
                       (make-push (make-curr-token #f))
                       (make-drop-token)
                       (make-go (make-label-name 's1)
                                '()))
                 (list (make-go (make-label-polynym 'r1
                                                    'have-token)
                                '()))))))))

(define s1-have-token/racket
  '(lambda (in tr regs stack)
     (case tr
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

(check-equal? (compile-insn-seq* s1-have-token) s1-have-token/racket)

(define s0-eos
  (list (make-block*
         (list (make-accept (list (make-named-reg 'reject)))))))

(define s0-eos/racket
  '(lambda (in tr regs stack)
     (list (dict-ref regs 'reject))))

(check-equal? (compile-insn-seq* s0-eos) s0-eos/racket)

(define s1-eos
  (list (make-block*
         (list (make-go (make-label-polynym 'r1
                                            'eos)
                        '())))))

(define s1-eos/racket
  '(lambda (in tr regs stack)
     ((r1-eos) in tr regs stack)))

(check-equal? (compile-insn-seq* s1-eos) s1-eos/racket)

(define r1
  (list (make-block*
         (list
          (make-assign (make-named-reg 'v) (make-pop))
          (make-assign (make-named-reg 'target) (make-pop)) ; state
          (make-sem-act 'r1
                        (list (make-named-reg 'v))
                        (list (make-named-reg 'ret-val))
                        'v)
          (make-push (make-named-reg 'target))
          (make-push (make-named-reg 'ret-val))
          (make-state-case (make-named-reg 'target)
                           (list (make-state 's0)
                                 (make-state 's1))
                           (list
                            (list
                             (make-go
                              (make-label-polynym 's1
                                                  'have-token)
                              '()))
                            (list
                             (make-go
                              (make-label-polynym 's0
                                                  'have-token)
                              '()))))))))

(define r1/racket
  '(lambda (in tr regs stack)
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

(check-equal? (compile-insn-seq* r1) r1/racket)

(define r1-eos
  (list (make-block*
         (list
          (make-assign (make-named-reg 'v) (make-pop))
          (make-assign (make-named-reg 'target) (make-pop)) ; state
          (make-sem-act 'r1
                        (list (make-named-reg 'v))
                        (list (make-named-reg 'ret-val))
                        'v)
          (make-push (make-named-reg 'target))
          (make-push (make-named-reg 'ret-val))
          (make-state-case (make-named-reg 'target)
                           (list (make-state 's0)
                                 (make-state 's1))
                           (list
                            (list
                             (make-go
                              (make-label-polynym 's1
                                                  'eos)
                              '()))
                            (list
                             (make-go
                              (make-label-polynym 's0
                                                  'eos)
                              '()))))))))

(define r1-eos/racket
  '(lambda (in tr regs stack)
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

(check-equal? (compile-insn-seq* r1-eos) r1-eos/racket)

;; compile-pdarisc
(check-equal? (compile-pdarisc
               (make-pdarisc
                (list (make-label
                       (list (make-label-name 's0)
                             (make-label-name 's1)
                             (make-label-polynym 's0 'have-token)
                             (make-label-polynym 's1 'have-token)
                             (make-label-polynym 's0 'eos)
                             (make-label-polynym 's1 'eos)
                             (make-label-name 'r1)
                             (make-label-polynym 'r1 'eos))
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
                       (list (make-go (make-label-name 's0) '()))))))
              `(lambda (in tr regs stack)
                 (letrec ((s0 (lambda ()
                                (lambda (in tr regs stack)
                                  (,s0/racket in tr regs stack))))
                          (s1 (lambda ()
                                (lambda (in tr regs stack)
                                  (,s1/racket in tr regs stack))))
                          (s0-have-token (lambda ()
                                           (lambda (in tr regs stack)
                                             (,s0-have-token/racket in tr regs stack))))
                          (s1-have-token (lambda ()
                                           (lambda (in tr regs stack)
                                             (,s1-have-token/racket in tr regs stack))))
                          (s0-eos (lambda ()
                                    (lambda (in tr regs stack)
                                      (,s0-eos/racket in tr regs stack))))
                          (s1-eos (lambda ()
                                    (lambda (in tr regs stack)
                                      (,s1-eos/racket in tr regs stack))))
                          (r1 (lambda ()
                                (lambda (in tr regs stack)
                                  (,r1/racket in tr regs stack))))
                          (r1-eos (lambda ()
                                    (lambda (in tr regs stack)
                                      (,r1-eos/racket in tr regs stack)))))
                   ((lambda (in tr regs stack)
                      ((s0) in tr regs stack))
                    in tr regs stack))))