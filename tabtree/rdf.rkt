#lang racket

(require rackunit)
(require odysseus)
(require odysseus/text)
(require (prefix-in query/ "query.rkt"))
(require (prefix-in parse/ "parse.rkt"))
(require (prefix-in utils/ "utils.rkt"))

(provide tabtree->rdf rdf-list?)

(define aliases (hash
    "a" "rdf/type"
    "instance-of" "rdf/type"
    "subclass-of" "rdfs/subClassOf"
    "subproperty-of" "rdfs/subPropertyOf"
    "alt" "owl/sameAs"
    "eq" "owl/sameAs"
    "eq-class" "owl/equivalentClass"
    "eq-property" "owl/equivalentProperty"
    "domain" "rdfs/domain"
    "range" "rdfs/range"
    "d" "rdfs/comment"
    "deabbr" "rdfs/label"
    "name" "rdfs/label"
))

;;; Tabtree -> RDF

(define (rdf-nil? v)
  (or (not v) (= v "rdf/nil")))

(define (rdf-list? v)
  (and (hash? v) ($ __rdf-list v)))

(define (from-tabtree-alias predicate)
  (hash-ref aliases predicate predicate))

(define (to-rdf-name s)
  (if (parse/namespaced? s)
    (format "~a:~a" (parse/namespace s) (parse/baseid s))
    (format ":~a" s)))

(define (object-value-str o)
  (cond
    ((rdf-list? o) (format "(~a)" (string-join (map object-value-str ($ __values o)) " ")))
    ((list? o) (string-join (map object-value-str o) ", "))
    ((url? o) (format "<~a>" o))
    ((string-in-string? o) (format "~a" o))
    ((number? o) (format "\"~a\"" o))
    (else (to-rdf-name o))))

(define (predicate-objects-str item)
  ; "makes rdf string of predicate-object pairs out of m hashmap"
  (let ((item-rdf (->>
                        item
                        ; parse/add-hi-rels
                        ; add-class
                        utils/remove-specials-extended)))
    (and
      (not (hash-empty? item-rdf))
      (->
        item-rdf
        ((λ (x)
          (for/fold
            ((res empty))
            (((p o) x))
            (pushr
              res
              (format " ~a ~a"
                (-> p from-tabtree-alias to-rdf-name)
                (object-value-str o))))))
        (string-join " ;\n")
        (string-append " .")))))

(define (clean-up-sections-refs tabtree)
  (define section-ids (list "classes" "properties" "individuals" "namespaces" "restrictions"))
  (utils/filter-map-tabtree
    (λ (v)
      (cond
        ((rdf-list? v)
          (let* ((old-values ($ __values v))
                (new-values (filter-not (λ (x) (index-of? section-ids x)) old-values)))
            (hash-union v (hash "__values" new-values))))
        ((list? v) (filter-not (λ (x) (index-of? section-ids x)) v))
        ((index-of? section-ids v) #f)
        (else v)))
    tabtree))

(define (extract-imports tabtree)
  (let* ((ontology-item (->> tabtree hash-values (filter (λ (x) (equal? ($ a x) "owl/Ontology"))) first))
        ; (_ (--- ontology-item))
        (imports (some->> tabtree
                      ($ namespaces)
                      ($ __children)
                      (map (λ (id) (hash-ref* tabtree id)))
                      ; (filter (λ (item) (hash-ref* item "ns")))))
                      (filter-not (λ (item) (hash-ref* item "no-prefix")))
                      (map (λ (item) (hash-ref* item "ns")))))
        (ontology-item (if (not-empty? imports)
                          (hash-union ontology-item (hash "owl/imports" imports))
                          ontology-item)))
    (hash-set tabtree ($ __id ontology-item) ontology-item)))

(define (tabtree->rdf tabtree)
  (let* (
        (tabtree (extract-imports tabtree))
        (namespaces (query/get-subtree '("namespaces") tabtree))
        (namespaces (hash-remove namespaces "namespaces"))
        (default-prefix (or
                          (some->>
                            tabtree
                            hash-values
                            (filter hash?)
                            (filter (λ (item) (equal? ($ a item) "owl/Ontology")))
                            only-or-first
                            ($ ns))
                          "https://example.org/ontology"))
        (header (for/fold
                  ((res (format "@prefix : <~a> .\n" default-prefix)))
                  (((prefix ns-item) namespaces))
                  (let ((prefix (if ($ "no-prefix" ns-item) "" prefix)))
                    (string-append
                      res
                      (format "@prefix ~a: <~a> .\n" (or prefix "") ($ ns ns-item))))))
        (tabtree (hash-remove-keys tabtree `("namespaces" ,@(hash-keys namespaces))))
        (tabtree (clean-up-sections-refs tabtree)))
    (for/fold
      ((res (format "~a\n\n" header)))
      ((id (sort
              (->> tabtree hash-keys (filter-not nil?))
              a-z)))
      (let ((form (predicate-objects-str (hash-ref tabtree id))))
        (if form
          (format "~a~a:~a\n~a\n\n" res (or (parse/namespace id) "") (parse/baseid id) form)
          res)))))
