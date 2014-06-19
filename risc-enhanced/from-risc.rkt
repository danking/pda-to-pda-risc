#lang racket
(require "data.rkt")
(require "../uid.rkt")
(provide convert/pdarisc)

(define-values
  (next-uid current-uid reset-uid! set-uid!)
  (init))

(define (convert/pdarisc pr)
  (match pr
    ((pdarisc uid reguid lbluid insn-seq*)
     (set-uid! uid)
     (let ((insns (convert/insn-seq* insn-seq*)))
       (pdarisc (current-uid) reguid lbluid insns)))))

(define (convert/insn i)
  (uninitialized-pda-term
   (match i
     ((assign uid id val) (assign uid (convert/register id) (convert/rhs val)))
     ((sem-act uid name in-vars out-vars action)
      (sem-act uid
               name
               (map convert/register in-vars)
               (map (lambda (maybe-r)
                      (if maybe-r
                          (convert/register maybe-r)
                          maybe-r))
                    out-vars)
               action))
     ((block uid insns) (block uid (convert/insn-seq insns)))
     ((push uid rhs)
      (block (next-uid)
             (list (uninitialized-pda-term (stack-ensure (next-uid) 1))
                   (uninitialized-pda-term (push uid (convert/rhs rhs))))))
     (_ i))))

(define (convert/insn* i)
  (uninitialized-pda-term
   (match i
     ((label uid ids st tt param-lists bodies body)
      (let ((converted-param-lists
             (map (lambda (param-list)
                    (map convert/register param-list))
                  param-lists))
            (converted-ids (map convert/label-name ids)))
       (label uid
              converted-ids
              st tt
              converted-param-lists
              (for/list ((body bodies)
                         (params converted-param-lists)
                         (id converted-ids))
                (cons (uninitialized-pda-term
                       (join-point (next-uid) id params))
                      (convert/insn-seq* body)))
              (convert/insn-seq* body))))
     ((block* uid seq)
      (block* uid (convert/insn-seq* seq)))
     ((accept uid regs)
      (accept uid (map convert/register regs)))
     ((reject uid) i)
     ((if-eos uid cnsq altr)
      (if-eos uid
              (convert/insn* cnsq)
              (convert/insn* altr)))
     ((state-case uid r l cnsqs)
      (state-case uid
                  (convert/register r)
                  l
                  (map convert/insn-seq* cnsqs)))
     ((token-case uid l cnsqs)
      (token-case uid l (map convert/insn-seq* cnsqs)))
     ((go uid target args)
      (go uid
          (convert/label-name target)
          (map convert/rhs args))))))

(define (convert/insn-seq seq)
  (map convert/insn seq))

(define (convert/insn-seq* seq)
  (match seq
    ((list i ... i*)
     (append (convert/insn-seq i)
             (list (convert/insn* i*))))))

(define (convert/label-name lbl)
  (let ((name (old:label-name-id lbl)))
    (uninitialized-label-name name)))

(define (convert/rhs rhs)
  (match rhs
    ((old:register) (convert/register rhs))
    ((state id) (state id))
    (_ rhs)))

(define (convert/register reg)
  (match reg
    ((old:named-reg id) (uninitialized-register id))
    ((old:nameless-reg) (uninitialized-register '_))))
