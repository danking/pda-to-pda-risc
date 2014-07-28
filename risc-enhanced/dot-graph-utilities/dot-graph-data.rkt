#lang racket

(provide empty-digraph named-empty-digraph
         set-global-node-attribute set-global-edge-attribute
         set-attribute add-edge add-node digraph->string)

(module+ test (require rackunit))

;; a Digraph is a (digraph Symbol
;; [SetOf Edge]
;; [SetOf Node]
;; [SetOf Digraph]
;; Attribute-Hash
;; Attribute-Hash
;; Attribute-Hash)
(struct digraph (name edge-set
                      node-set
                      sub-graphs
                      ;; attribute hashes
                      node-attributes
                      edge-attributes
                      attributes))
(define empty-digraph (digraph "G"
                               (set) (set) (set)
                               (hash) (hash) (hash)))
(define (named-empty-digraph name) (digraph name
                                            (set) (set) (set)
                                            (hash) (hash) (hash)))

;; An Edge is a (edge Symbol Symbol Attribute-Hash)
(struct edge (from to attributes))

;; An Attribute-Hash is a [Hash String (U String Number Symbol)]

;; A Node is a (node Symbol Attribute-Hash)
(struct node (name attributes))

(define (set-attribute g attribute value)
  (match g
    ((digraph name es ns sgs node-attributes edge-attributes attributes)
     (digraph name
              es ns sgs
              node-attributes edge-attributes (hash-set attributes attribute value)))))

(define (set-global-node-attribute g attribute value)
  (match g
    ((digraph name es ns sgs node-attr edge-attr attributes)
     (digraph name
              es ns sgs
              (hash-set node-attr attribute value) edge-attr attributes))))

(define (set-global-edge-attribute g attribute value)
  (match g
    ((digraph name es ns sgs node-attr edge-attr attributes)
     (digraph name
              es ns sgs
              node-attr (hash-set edge-attr attribute value) attributes))))

(define (add-edge g from to [attributes (hash)])
  (match g
    ((digraph name es ns sgs node-attr edge-attr attr)
     (digraph name
              (set-add es (edge from to attributes)) ns sgs
              node-attr edge-attr attr))))

(define (add-node g node-name [attributes (hash)])
  (match g
    ((digraph graph-name es ns sgs node-attr edge-attr attr)
     (digraph graph-name
              es (set-add ns (node node-name attributes)) sgs
              node-attr edge-attr attr))))

(define (add-subgraph g sg)
  (match g
    ((digraph name es ns sgs node-attr edge-attr attr)
     (digraph name
              es ns (set-add sgs sg)
               node-attr edge-attr attr))))

(define (digraph->string g [indent ""])
  (graph->string g "digraph" indent))
(module+ test
  (check-equal? (digraph->string (add-node (add-edge empty-digraph
                                                     'foo 'bar)
                                           'foo (hash 'color "0 0 255")))
"digraph G {
foo [color = \"0 0 255\", ];
foo -> bar [];
}\n")
  (check-equal? (digraph->string
                 (add-subgraph (add-node (add-edge empty-digraph
                                                   'foo 'bar)
                                         'foo (hash 'color "0 0 255"))
                               (add-edge (named-empty-digraph "cluster")
                                         'baz 'qux)))
"digraph G {
subgraph cluster {
baz -> qux [];
}

foo [color = \"0 0 255\", ];
foo -> bar [];
}\n")
  (check-equal? (digraph->string
                 (set-global-edge-attribute (add-edge empty-digraph
                                                      'foo 'bar)
                                            'foo "red"))
"digraph G {
edge [foo = \"red\", ];
foo -> bar [];
}\n"))

(define (subgraph->string g [indent ""])
  (graph->string g "subgraph" indent))
(module+ test
  (check-equal? (subgraph->string (add-edge (named-empty-digraph "cluster")
                                            'baz 'qux))
"subgraph cluster {
baz -> qux [];
}
"))

(define (graph->string g keyword indent)
  (let ((sub-indent (string-append " " indent)))
    (match g
      ((digraph name es ns sgs node-attr edge-attr attr)
       (string-append indent keyword " " name " {\n"
                      ;; attributes
                      (graph-attributes->string attr sub-indent)
                      (tagged-attribute-list "node" node-attr sub-indent)
                      (tagged-attribute-list "edge" edge-attr sub-indent)
                      ;; subgraphs
                      (subgraphs->string sgs sub-indent)
                      ;; nodes and edges
                      (indented-block sub-indent node->string ns)
                      (indented-block sub-indent edge->string es)
                      indent "}\n")))))

(define (subgraphs->string sub-graphs indent)
  (indented-block ""
                  (lambda (sg)
                    (subgraph->string sg indent))
                  sub-graphs
                  "\n"))

(define (graph-attributes->string attributes indent)
  (for/fold
      ((s ""))
      (((k v) attributes))
    (string-append s indent (attribute->string k v) ";\n")))

(define (indented-block indent formatter elements [newline ";\n"])
  (for/fold
      ((s ""))
      ((e elements))
    (string-append s indent (formatter e) newline)))

;; hash-empty? : [Hash Any Any] -> Boolean
;;
;; produces true if the given has has no key-value pairs
(define (hash-empty? h)
  (= 0 (hash-count h)))
(module+ test
  (check-true (hash-empty? (hash)))
  (check-false (hash-empty? (hash 3 4))))

;; tagged-attribute-list : String Attribute-Hash {Optional String} -> String
;;
;; Produces an attribute list with desired indentation and a "tag" a la "node"
;; or "edge".
(define (tagged-attribute-list tag attributes [indent ""])
  (if (hash-empty? attributes)
      ""
      (string-append indent tag " " (attributes->attributes-list attributes) ";\n")))
(module+ test
  (check-equal? (tagged-attribute-list "edge" (hash))
                "")
  (check-equal? (tagged-attribute-list "edge" (hash 'foo 3))
                "edge [foo = 3, ];\n")
  (check-equal? (tagged-attribute-list "edge" (hash 'foo 3) " ")
                " edge [foo = 3, ];\n"))

(define (attribute->string k v)
  (string-append (symbol->string k) " = " (outputify v)))
(module+ test
  (check-equal? (attribute->string 'size "4,4") "size = \"4,4\"")
  (check-equal? (attribute->string 'weight 3) "weight = 3"))

(define (attributes->attributes-list attributes)
  (string-append "["
                 (for/fold ((s "")) (((k v) attributes))
                   (string-append (attribute->string k v) ", " s))
                 "]"))
(module+ test
  (check-equal? (attributes->attributes-list (hash))
                "[]")
  (check-equal? (attributes->attributes-list (hash 'color "0 0 255"
                                                   'size "4,4"))
                "[size = \"4,4\", color = \"0 0 255\", ]"))

(define (node->string n)
  (match n
    ((node name attributes)
     (string-append (symbol->string name) " "
                    (attributes->attributes-list attributes)))))
(module+ test
  (check-equal? (node->string (node 'foo (hash)))
                "foo []")
  (check-equal? (node->string (node 'foo (hash 'color "0 0 255")))
                "foo [color = \"0 0 255\", ]"))

(define (edge->string e)
  (match e
    ((edge from to attributes)
     (string-append (symbol->string from) " -> " (symbol->string to) " "
                    (attributes->attributes-list attributes)))))
(module+ test
  (check-equal? (edge->string (edge 'from 'to (hash)))
                "from -> to []")
  (check-equal? (edge->string (edge 'from 'to (hash 'weight 8)))
                "from -> to [weight = 8, ]")
  (check-equal? (edge->string (edge 'from 'to (hash 'weight 8 'color "255 255 0")))
                "from -> to [weight = 8, color = \"255 255 0\", ]"))

(define (outputify v)
  (cond [(string? v) (string-replace (format "~s" v)
                                     ;; format will tfurn special \l character
                                     ;; sequences into escaped \\l which
                                     ;; prevents dot from properly aligning
                                     ;; text
                                     "\\\\l"
                                     "\\l")]
        [(number? v) (number->string v)]
        [(symbol? v) (symbol->string v)]))
(module+ test
  (check-equal? (outputify "foo") "\"foo\"")
  (check-equal? (outputify 3) "3")
  (check-equal? (outputify 'bar) "bar"))
