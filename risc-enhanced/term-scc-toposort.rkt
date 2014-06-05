#lang racket

(require graph)
(provide pda-term-scc-priority-hash)

;; pda-term-scc-priority-hash : PDA-RISC-ENH -> [Dict PDA-RISC-ENH Natural]
(define (pda-term-scc-priority-hash t)
  (let* ((sccs (scc t))
         (sccs-graph (create-scc-graph sccs))
         (ordered-sccs (tsort sccs-graph)))
    ;; We use a mutable hash here because once we're done creating it, we never
    ;; modify it and it *must* be fast. A priority queue is the fundamental data
    ;; structure in our analysis, we're constantly removing and adding things to
    ;; it.
    (define priority-map (make-hash '()))

    (for ([i (in-naturals 0)]
          [scc ordered-sccs])
      (for ([term scc])
        (dict-set! priority-map term i)))

    priority-map))

(define (create-scc-graph sccs)
  (define scc-graph (unweighted-graph/directed '()))
  (define sccs-as-sets
    (for/list ([scc sccs]) (for/mutable-set ([term scc]) term)))

  (for ([scc sccs-as-sets])
    (for ([term scc])
      (for ([neighbor (in-neighbors term term)]
            #:when (not (set-member? scc neighbor)))
        (let ((neighbors-scc (findf (lambda (x) (set-member? x neighbor))
                                    sccs-as-sets)))
          (unless neighbors-scc
            (error 'create-scc-graph
                   (format "The node ~a doesn't have an scc?!" neighbor)))
          (add-edge! scc-graph scc neighbors-scc)))))

  scc-graph)
