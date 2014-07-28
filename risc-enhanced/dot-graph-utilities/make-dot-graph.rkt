#lang racket

(require "../search.rkt"
         "../data.rkt"
         "dot-graph-data.rkt")
(provide make-dot-graph term->node-name uid->node-name)

;; make-dot-graph : PDA-RISC
;; (Term -> Attribute-Hash)
;; (Term -> String)
;; ->
;; Digraph
;;
;; Produces a visually appealing Graphviz dot-langauge compatible Digraph from a
;; pda-risc term by traversing the graph.
(define (make-dot-graph pre
                        #:attrs [attrs (lambda (_) (hash))]
                        #:term->text [term->text textify])
  ;; combine : Term Digraph -> Digraph
  ;;
  ;; Combine only adds the current term to the node set, but adds all the
  ;; outgoing edges to the edge set. We don't add all the successors themselves
  ;; to the node set now because they will be proccessed later.
  (define (combine t g)
    (let* ((g (add-term-node g t attrs term->text))
           (succs (pda-term-succs t)))
      (add-valid-succ-edges g t succs)))

  (let ((empty-graph-with-settings
         (set-global-node-attribute empty-digraph
                                    'fontname
                                    "monospace")))
    (folding-forward-search combine
                            empty-graph-with-settings
                            (pda-risc-enh-initial-term pre))))

;; make-8.5x11-graph : PDA-RISC -> Digraph
;;
;; Produces a digraph which will fit on one US letter page if using the
;; Postscript output of dot
(define (make-8.5x11-graph pre)
  (set-attribute (make-dot-graph pre)
                 'page
                 "8.5,11"))

;; add-valid-succ-edges : Digraph Term [SetOf Term] -> Digraph
;;
;; Add to the digraph a representation of the terms in succs that are
;; `valid-term?'. The representing nodes' names are defined by
;; `term->node-name'. Their "styling", i.e. their label and color, is defined by
;; `add-term-node'.
(define (add-valid-succ-edges g t succs)
  (for/fold
      ((g g))
      ((succ succs)
       #:when (valid-term? succ))
    (add-edge g
              (term->node-name t)
              (term->node-name succ)
              (hash))))

;; valid-term? : Term -> Boolean
;;
;; A term is considered `valid' if its printed form has necessary information
;; for understanding the pda. In the case of block and block* this is
;; untrue.
(define (valid-term? term)
  #t
  ;; (let ((i (pda-term-insn term)))
  ;; (not (or (block? i)
  ;; (block*? i)
  ;; (label? i))))
  )

;; string->left-aligned-string : String -> String
;;
;; Dot has three different escape sequences for newline. It uses \n, \l, and
;; \r. These create center-aligned, left-aligned, and right-aligned lines,
;; respectively.
(define (string->left-aligned-string s)
  (string-replace s "\n" "\\l"))

(define (term->node-name t)
  (uid->node-name (get-uid (pda-term-insn t))))

(define (uid->node-name uid)
  (string->symbol
   (string-append "id" (number->string uid))))

;; add-term-node : Digraph
;; Term
;; (Term -> Attribute-Hash)
;; (Term -> String)
;; ->
;; Digraph
;;
;; Converts a term into a digraph node representation. A term is represented by
;; a node whose label is defined by `textify'. The `term->node-name' procedure
;; produces a uniform naming scheme to allow matching nodes with edges.
(define (add-term-node g t attrs term->text)
  (let ((text (term->text t))
        (name (term->node-name t)))
    (add-node g
              name
              (hash-set (term-specific-attributes t attrs)
                        'label text))))

;; term-specific-attributes : Term (Term -> Attribute-Hash) -> Attribute-Hash
;;
;; This procedure allows us to give certain terms visually distinguishing
;; attributes.
(define (term-specific-attributes t attrs)
  (let* ((i (pda-term-insn t))
         (etc (attrs t)))
    (cond [(push? i)
           (hash-set etc 'shape "triangle")]
          [(pop-assign? i)
           (hash-set etc 'shape "invtriangle")]
          [else etc])))

(define (textify t)
  (shorten (term->string t)))

;; term->string : Term -> String
;;
;; Converts a term into a string for dot labels by producing a s-expression,
;; then using the pretty-printer to produce a string and then converts the
;; string to a left-aligned string (by dot convetions).
;;
;; uses the risc-enhnaced/data.rkt unparser
(define term->string
  (compose string->left-aligned-string
           pretty-format
           pre-term->risc-sexp))

(define (shorten s)
  (substring s 0 (min 100 (string-length s))))
