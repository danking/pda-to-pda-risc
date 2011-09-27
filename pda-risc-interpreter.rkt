#lang racket
(require
 (file "/home/danking/development/personal-scheme-hacks/record.rkt"))

(provide (all-defined-out))

(define-ustruct machine
  (current-state current-token stack labels token-stream env)
  #:transparent)
(define-struct state (id) #:transparent)
(define-struct nterm (id) #:transparent)

(define (start insn strm)
  (interpret insn
             (machine #f #f '() (hasheq) strm (hasheq))
             (lambda (x) x)))

(define (interpret insn m k)
  (printf "current insn: ~a\n" insn)
  (match insn
    ((list 'pop)
     (pop m k))
    ((list 'state st)
     (k (state st) m))
    ((list 'nterm nt)
     (k (nterm nt) m))
    ((list 'current-token)
     (current-token m k))
    ((list ':= var val)
     (interpret val
                m
                (lambda (val m) (:= var val m k))))
    ((list 'push val)
     (interpret val
                m
                (lambda (val m) (push val m k))))
    ((list 'semantic-action (list in-vars ...)
                            (list out-vars ...)
                            action)
     (semantic-action in-vars out-vars action m k))
    ((list 'stack-ensure _)
     (k #f))
    ((list 'block insns ...)
     (block insns m k))
    ((list 'label (list bindings ...) body ...)
     (label bindings body m k))
    ((list 'accept var)
     (accept var m k))
    ((list 'if-eos a b)
     (if-eos a b m k))
    ((list 'state-case var pairs ...)
     (state-case var pairs m k))
    ((list 'token-case pairs ...)
     (token-case pairs m k))
    ((list 'nterm-case var pairs ...)
     (nterm-case var pairs m k))
    ((list 'go target args ...)
     (go target args m k))
    ((list insns ...)
     (block insns m k))
    ('drop-token
     (drop-token m k))
    ('get-token
     (get-token m k))
    (var
     (get-var var m k))))

(define (get-var var m k)
  (k (hash-ref (machine-env m)
               var
               (lambda ()
                 (error 'get-var
                        "variable ~a is used before its definition env: ~a"
                        var
                        (machine-env m))))
     m))

(define (pop m k)
  (k (car (machine-stack m))
     (machine-update-stack (cdr (machine-stack m)) m)))

(define (current-token m k)
  (k (machine-current-token m)
     m))

(define (:= var val m k)
  (k (machine-update-env (hash-set (machine-env m)
                                   var val)
                         m)))

(define (push e m k)
  (k (machine-update-stack (cons e (machine-stack m)) m)))

(define (semantic-action in-vars out-vars action m k)
  (k (let ((out-vals (external-evaluate action
                                        in-vars
                                        (map (lambda (id)
                                               (hash-ref (machine-env m) id))
                                             in-vars))))
       (machine-update-env* out-vars out-vals m))))

(define (external-evaluate action params args)
  (call-with-values (lambda ()
                      (parameterize ([current-namespace (make-base-namespace)])
                        (eval `(apply (lambda ,params ,action) ',args))))
                    list))

(define (drop-token m k)
  (k (machine-update-token-stream (cdr (machine-token-stream m)) m)))

(define (get-token m k)
  (k (machine-update-current-token (car (machine-token-stream m)) m)))

(define (label bindings body m k)
  (interpret body
             (machine-update-labels (foldl (lambda (id val hsh)
                                             (hash-set hsh id val))
                                           (machine-labels m)
                                           (map car bindings)
                                           (map cdr bindings))
                                    m)
             k))

(define (block insns m k)
  (if (empty? insns)
      (k m)
      (interpret (car insns)
                 m
                 (lambda (m) (block (cdr insns) m k)))))

(define (accept var m k)
  (get-var var
           m
           list))

(define (if-eos a b m k)
  (if (empty? (machine-token-stream m))
      (interpret a m k)
      (interpret b m k)))

(define (state-case var pairs m k)
  (get-var var m
           (lambda (st m)
             (let ((insns (assq (state-id st) pairs)))
               (if insns
                   (interpret (cdr insns) m k)
                   (error 'state-case
                          "no match for ~a found in ~a" st pairs))))))

(define (token-case pairs m k)
  (let ((insns (case-clause-lookup (machine-current-token m) pairs)))
    (if insns
        (interpret (cdr insns) m k)
        (error 'token-case "no match for ~a found in ~a"
               (machine-current-token m)
               pairs))))

(define (nterm-case var pairs m k)
  (get-var var m
           (lambda (nt m)
             (let ((insns (assq (nterm-id nt) pairs)))
               (if insns
                   (interpret (cdr insns) m k)
                   (error 'nterm-case
                          "no match for ~a found in ~a" nt pairs))))))

(define (map-cps* p ls m k)
  (if (empty? ls)
      (k '() m)
      (p (car ls)
         m
         (lambda (v m)
           (map-cps* p
                     (cdr ls)
                     m
                     (lambda (vs m)
                       (k (cons v vs) m)))))))

(define (go target args m k)
  (map-cps* interpret
            args
            m
            (lambda (args m)
              (printf "*** args: ~a" args)
              (let ((params+insns (hash-ref (machine-labels m) target #f)))
                (if params+insns
                    (interpret (cdr params+insns)
                               (machine-update-env* (car params+insns) args m)
                               k)
                    (error 'go "label ~a was not found. labels: ~a" target
                           (machine-labels m)))))))

(define (machine-update-env* vars vals m)
  (machine-update-env (foldl (lambda (id val hsh)
                               (hash-set hsh id val))
                             (machine-env m)
                             vars
                             vals)
                      m))

;; case-clause-lookup : Symbol [ListOf CaseClause] -> (U #f CaseClause)
(define (case-clause-lookup v clauses)
  (or (assq v clauses)
      (assq #t clauses)))
