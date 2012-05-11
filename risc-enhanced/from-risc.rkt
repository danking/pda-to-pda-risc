#lang racket
(require "../pdarisc-data.rkt"
         (prefix-in enh: "data.rkt"))
(provide convert/pdarisc)

(define (convert/pdarisc pr)
  (match pr
    ((pdarisc insn-seq*)
     (pdarisc (convert/insn-seq* insn-seq*)))))

(define (convert/insn i)
  (enh:uninitialized-pda-term
   (match i
     ((assign id val) (assign (convert/register id) (convert/rhs val)))
     ((sem-act name in-vars out-vars action)
      (sem-act name
               (map convert/register in-vars)
               (map (lambda (maybe-r)
                      (if maybe-r
                          (convert/register maybe-r)
                          maybe-r))
                    out-vars)
               action))
     ((block insns) (block (convert/insn-seq insns)))
     ((push rhs) (push (convert/rhs rhs)))
     (_ i))))

(define (convert/insn* i)
  (enh:uninitialized-pda-term
   (match i
     ((label ids st tt param-lists bodies body)
      (let ((converted-param-lists
             (map (lambda (param-list)
                    (map convert/register param-list))
                  param-lists))
            (converted-ids (map convert/label-name ids)))
       (label converted-ids
              st tt
              converted-param-lists
              (for/list ((body bodies)
                         (params converted-param-lists)
                         (id converted-ids))
                (cons (enh:uninitialized-pda-term
                       (enh:join-point id params))
                      (convert/insn-seq* body)))
              (convert/insn-seq* body))))
     ((block* seq)
      (block* (convert/insn-seq* seq)))
     ((accept regs)
      (accept (map convert/register regs)))
     ((reject) i)
     ((if-eos cnsq altr)
      (if-eos (convert/insn* cnsq)
              (convert/insn* altr)))
     ((state-case r l cnsqs)
      (state-case (convert/register r)
                  l
                  (map convert/insn-seq* cnsqs)))
     ((token-case l cnsqs)
      (token-case l (map convert/insn-seq* cnsqs)))
     ((go target args)
      (go (convert/label-name target)
          (map convert/rhs args))))))

(define (convert/insn-seq seq)
  (map convert/insn seq))

(define (convert/insn-seq* seq)
  (match seq
    ((list i ... i*)
     (append (convert/insn-seq i)
             (list (convert/insn* i*))))))

(define (convert/label-name lbl)
  (let ((name (label-name->symbol lbl)))
    (enh:uninitialized-label-name name)))

(define (convert/rhs rhs)
  (match rhs
    ((register) (convert/register rhs))
    (_ rhs)))

(define (convert/register reg)
  (match reg
    ((named-reg id) (enh:uninitialized-register (syntax-e id)))
    ((nameless-reg) (enh:uninitialized-register '_))))
