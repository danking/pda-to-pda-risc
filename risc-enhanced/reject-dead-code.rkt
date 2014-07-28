#lang racket

(require "data.rkt" "filter.rkt")
(provide get-all-doomed-sequences
         get-all-rejects
         remove-all-doomed-sequences!)

(define (get-all-rejects pre)
  (filter-to-list pre (compose reject? pda-term-insn)))

(define (get-all-doomed-sequences pre)
  (let ((rejects (get-all-rejects pre)))
    (map (lambda (reject)
           (list (most-recent-control-split/join reject) reject))
         rejects)))

(define (remove-all-doomed-sequences! pre)
  (for ((reject (get-all-rejects pre)))
    (remove-doomed-sequence! reject)))

;; Remove all predecssors of the reject which do not have multiple successors or
;; predecessors
(define (remove-doomed-sequence! reject)
  (let loop ((t (set-first (pda-term-preds reject))))
    (cond [(control-split/join? t) (void)]
          [else (let ((next-term (set-first (pda-term-preds t))))
                  (elide-term! t)
                  (loop next-term))])))

;; removes a term from the graph by
;; - removing itself from its predecssors' successor sets
;; - adding its successors to its predecessors' successor sets
;; - removing all links that it has (so it can be GC'd)
(define (elide-term! to-elide)
  (remove-link-from-preds! to-elide)
  (remove-link-from-succs! to-elide)
  (set-succs-to-preds-succs! to-elide)
  (set-preds-to-succs-preds! to-elide)
  (remove-outgoing-links-from-term! to-elide))

(define (remove-link-from-preds! to-elide)
  (for ([pred (pda-term-preds to-elide)])
    (set-pda-term-succs! pred (set-remove (pda-term-succs pred) to-elide))))
(define (remove-link-from-succs! to-elide)
  (for ([succ (pda-term-succs to-elide)])
    (set-pda-term-preds! succ (set-remove (pda-term-preds succ) to-elide))))

(define (set-succs-to-preds-succs! to-elide)
  (for ([pred (pda-term-preds to-elide)])
    (set-pda-term-succs! pred (set-union (pda-term-succs pred)
                                         (pda-term-succs to-elide)))))
(define (set-preds-to-succs-preds! to-elide)
  (for ([succ (pda-term-succs to-elide)])
    (set-pda-term-preds! succ (set-union (pda-term-preds succ)
                                         (pda-term-preds to-elide)))))

(define (remove-outgoing-links-from-term! t)
  (set-pda-term-preds! t (seteq))
  (set-pda-term-succs! t (seteq))
  (set-pda-term-avail-regs! t #f)
  (set-pda-term-live-regs! t #f))

(define (most-recent-control-split/join t)
  (cond [(control-split/join? t) t]
        [else (most-recent-control-split/join (set-first (pda-term-preds t)))]))

(define (control-split/join? t)
  (or (> (set-count (pda-term-preds t)) 1)
      (> (set-count (pda-term-succs t)) 1)))
