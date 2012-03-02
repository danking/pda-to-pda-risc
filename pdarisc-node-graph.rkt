#lang racket
(require "pdarisc-data.rkt")
(provide node reg-node join-node build-node-graph)

;; A Key is an ExactInteger

;; get-next-key : Unit -> Key
;; rest-key-counter : Unit -> Unit
(define-values (get-next-key reset-key-counter)
  (let ((counter -1))
    (values (lambda () (set! counter (add1 counter)) counter)
            (lambda () (set! counter -1)))))

;; list-of-keys : [ListOf Any] -> [ListOf Key]
;; produces a list of keys equal in length to the given list
(define (list-of-keys ls)
  (map (lambda (_) (get-next-key)) ls))

;; A LabelEnv is a [Hash LabelName Key]
(define (label-env-set env key val)
  (hash-set env (label-name->symbol key) val))
(define (label-env-get env key)
  (hash-ref env (label-name->symbol key)))

;; A Node is a (node [ListOf Key])
(define-struct node (next-node) #:transparent)
(define-struct (reg-node node) (insn) #:transparent)
(define-struct (join-node node) (arglist) #:transparent)

;; A Graph is a [Hash Key Node]

;; build-node-graph : PDA-RISC -> Graph
(define (build-node-graph pdarisc)
  (process (pdarisc-insns pdarisc) (hash) (hash) (get-next-key)))

;; process : Insn*-Seq Graph LabelEnv Key -> Graph
(define (process seq graph label-env my-key)
  (if (empty? (rest seq))
      (process-insn* (first seq) graph label-env my-key)
      (process-insn (first seq) (rest seq) graph label-env my-key)))

;; process-insn* : Insn* Graph LabelEnv Key -> Graph
(define (process-insn* i* graph label-env my-key)
  (match i*
    ((block* subseq)
     (process subseq graph label-env my-key))
    ((label names stack-types token-types
            arglists branches body)
     (process-label i* graph label-env my-key))
    ((accept lor)
     (hash-set graph my-key (reg-node '() i*)))
    ((reject)
     (hash-set graph my-key (reg-node '() i*)))
    ((if-eos cnsq altr)
     (let ((successor-keys (list (get-next-key)
                                 (get-next-key))))
       (process-insn* altr
                      (process-insn* cnsq
                                     (hash-set graph my-key (reg-node successor-keys i*))
                                     label-env
                                     (first successor-keys))
                      label-env
                      (second successor-keys))))
    ((or (state-case _ _ cnsqs)
         (token-case _ cnsqs))
     (let ((successor-keys (list-of-keys cnsqs)))
       (process/many cnsqs
                     (hash-set graph my-key (reg-node successor-keys i*))
                     label-env
                     successor-keys)))
    ((go label _)
     (hash-set graph my-key (reg-node (list (label-env-get label-env label)) i*)))))

;; process/many : [ListOf Insn*-Seq] Graph LabelEnv [ListOf Keys] -> Graph
(define (process/many seqs graph labelenv keys)
  (for/fold ((graph graph))
            ((seq seqs)
             (key keys))
    (process seq graph labelenv key)))

;; process-label : Label
;;                 Graph
;;                 LabelEnv
;;                 Key
;;                 ->
;;                 Graph
(define (process-label i* graph label-env my-key)
  (match-define (label names _ _ arglists branches body) i*)

  (let* ((next-keys (list-of-keys names))
         (label-env* (for/fold ((env label-env))
                         ((next-key next-keys)
                          (name names))
                       (label-env-set env name next-key)))
         (body-key (get-next-key))
         (graph* (hash-set graph my-key (reg-node (list body-key) i*))))
    (process body
             (for/fold ((new-graph graph*))
                 ((next-key next-keys)
                  (arglist arglists)
                  (branch branches))
               (let ((key-after-join (get-next-key)))
                 (hash-set (process branch new-graph label-env* key-after-join)
                           next-key
                           (join-node (list key-after-join) arglist))))
             label-env*
             body-key)))

;; process-insn : Insn Insn*-Seq Graph LabelEnv Key -> Graph
(define (process-insn i seq graph label-env my-key)
  (match i
    ((block subseq)
     (process (append subseq seq) graph label-env my-key))
    ((insn)
     (let ((next-key (get-next-key)))
       (process seq
                (hash-set graph
                          my-key
                          (reg-node (list next-key)
                                    i))
                label-env
                next-key)))))