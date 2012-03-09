#lang racket
(require "pdarisc-data.rkt"
         "node-graph.rkt")
(provide join-node build-node-graph)

;; an NG is a [NodeGraph [U Insn Insn*]]
;; some auxiliary functions to NG may return values of type Key

;; build-node-graph : PDA-RISC -> NG
(define (build-node-graph pdarisc)
  (let-values
      (((ng source-key)
        (process (pdarisc-insns pdarisc) (make-node-graph) (hash))))
    (node-graph-set-source/key ng source-key)))

;; process : Insn*-Seq NG LabelEnv -> (values NG Key)
(define (process seq ng label-env)
  (if (empty? (rest seq))
      (process-insn* (first seq) ng label-env)
      (process-insn (first seq) (rest seq) ng label-env)))

;; process-insn* : Insn* NG LabelEnv -> (values NG Key)
(define (process-insn* i* ng label-env)
  (match i*
    ((block* subseq)
     (process subseq ng label-env))
    ((label names stack-types token-types
            arglists branches body)
     (process-label i* ng label-env))
    ((or (accept _)
         (reject))
     (node-graph-add-node ng i*))
    ((if-eos cnsq altr)
     ;; note that cnsq and atlr are Insn*s, so we turn them into Insn*-seqs
     ;; (the inner calls to list), and put them into a list (the outer call
     ;; to list)
     (let-values (((ng succ-keys) (process-many (list (list cnsq) (list altr))
                                                ng label-env)))
       (node-graph-add-node/succs ng i* succ-keys)))
    ((or (state-case _ _ cnsqs)
         (token-case _ cnsqs))
     (let-values (((ng succ-keys) (process-many cnsqs ng label-env)))
       (node-graph-add-node/succs ng i* succ-keys)))
    ((go label _)
     (node-graph-add-node/succs ng i* (list (label-env-get label-env label))))))

;; process-many : [ListOf Insn*-Seq] NG LabelEnv -> (values NG [SetOf Key])
(define (process-many seqs ng labelenv)
  (for/fold ((ng ng)
             (keys (set)))
            ((seq seqs))
    (let-values
        (((ng key) (process seq ng labelenv)))
      (values ng (set-add keys key)))))

;; process-label : Label
;;                 NG
;;                 LabelEnv
;;                 ->
;;                 (values NG Key)
(define (process-label i* ng label-env)
  (match-define (label branch-names _ _ arglists branches body) i*)

  (let* ((label-env/boxes (for/fold ((env label-env))
                                    ((name branch-names))
                            (label-env-set env name (box #f)))))
    (let-values
        (((ng/branches branch-keys)
          (for/fold ((ng ng)
                     (keys (set)))
              ((arglist arglists)
               (branch branches))
            (let*-values
                (((ng* branch-key) (process branch ng label-env/boxes))
                 ((ng** join-key) (node-graph-add-node/succs ng*
                                                             (join-node arglist)
                                                             (list branch-key))))
              (values ng** (set-add keys join-key))))))
      (for ((name branch-names)
            (key branch-keys))
        (set-box! (label-env-get label-env/boxes name) key))
      (process body
               ng/branches
               label-env/boxes))))

;; process-insn : Insn Insn*-Seq NG LabelEnv -> (values NG Key)
(define (process-insn i seq ng label-env)
  (match i
    ((block subseq)
     (process (append subseq seq) ng label-env))
    ((insn)
     (let-values
         (((ng succ-key)
           (process seq ng label-env)))
       (node-graph-add-node/succs ng i (list succ-key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;; A join-node is a dummy node used for join points
(define-struct join-node (arglist) #:transparent)

;; A LabelEnv is a [Hash LabelName [U Key [Box Key]]]

;; label-env-set : LabelEnv -> LabelEnv
(define (label-env-set env key val)
  (hash-set env (label-name->symbol key) val))
;; label-env-set : LabelEnv -> Symbol
(define (label-env-get env key)
  (hash-ref env (label-name->symbol key)))

