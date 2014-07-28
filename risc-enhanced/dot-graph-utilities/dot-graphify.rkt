#lang racket

(require "make-dot-graph.rkt"
         "dot-graph-data.rkt"
         "../decorate.rkt"
         (only-in "../data.rkt"
                  pda-risc-enh-initial-term
                  pda-term->uid))
(provide (all-defined-out))

(define (dot-graphify pre outfile attr naming)
  (with-output-to-file outfile
    (lambda ()
      (display
       (digraph->string
        (make-dot-graph pre
                        #:attrs attr
                        #:term->text naming))))
    #:exists 'replace))

(define (visited-coloring s)
  (lambda (term)
    (if (set-member? s (pda-term->uid term))
        (hash)
        (hash 'style "filled"
              'fillcolor "red"))))

(define uid-naming
  (lambda (term)
    (format "~a" (pda-term->uid term))))
