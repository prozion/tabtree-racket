#lang racket

(require rackunit)
(require odysseus)
(require odysseus/text)
(require odysseus/time)
(require (prefix-in query/ "query.rkt"))
(require (prefix-in parse/ "parse.rkt"))
(require (prefix-in utils/ "utils.rkt"))

(require "globals.rkt")
(require "basic-ontologies.rkt")

(provide tabtree->rdf rdf-list?)

(define XSD_NAMESPACE "http://www.w3.org/2001/XMLSchema#")

(define Tabtree (make-parameter (hash)))
(define PTypes (make-parameter (hash)))
(define XSD_NS (make-parameter "xsd"))

;;; Tabtree -> RDF

(define (rdf-nil? v)
  (or (not v) (= v "rdf/nil")))

(define (rdf-list? v)
  (and (hash? v) ($ __rdf_list v)))

(define (from-tabtree-alias predicate)
  (hash-ref aliases predicate predicate))

(define (get-o-type p)
  (or
    (hash-ref (PTypes) p #f)
    (let* ((p-item (hash-ref (Tabtree) p #f))
          (p-alias (hash-ref aliases p p))
          (p-item (or p-item (hash-ref BASIC_ONTOLOGIES p-alias (hash))))
          (p-range (hash-ref-some p-item '("range" "rdfs/range") #f))
          (o-type (if p-range
                    (-> p-range (string-split "/") last)
                    (let ((p-superproperty-id (hash-ref p-item "subproperty-of" #f)))
                      (if p-superproperty-id
                        (get-o-type p-superproperty-id)
                        "Unknown")))))
      (PTypes (hash-set (PTypes) p o-type))
      o-type)))

(define-catch (object-value-str o p)
  (define object-value-str-p (λ (o) (object-value-str o p)))
  (cond
    ((rdf-list? o) (format "(~a)" (string-join (map object-value-str-p ($ __values o)) " ")))
    ((list? o) (string-join (map object-value-str-p o) ", "))
    ((string-in-string? o) (format "~a" o))
    (else
      (let* ((o-type (get-o-type p))
            (p-item (hash-ref (Tabtree) p (hash))))
        (cond
          ((equal? p "rdf/object")
            (let* ((statement-item (->> (Tabtree) hash-values (filter (λ (item) (equal? ($ rdf/object item) o))) first-or-only))
                  (predicate ($ rdf/predicate statement-item)))
              (object-value-str o predicate)))
          (else
            (cond
              ; Tree of XML Schema datatypes: https://www.w3.org/TR/xmlschema-2/#built-in-datatypes
              ((index-of? '("Url" "Ontology") o-type) (format "<~a>" o))
              ((index-of? '("Boolean") o-type) (format "\"~a\"^^~a:boolean" o (XSD_NS)))
              ((index-of? '("Integer") o-type) (format "\"~a\"^^~a:integer" (utils/parse-shorthand-value o) (XSD_NS)))
              ((index-of? '("PositiveInteger") o-type) (format "\"~a\"^^~a:positiveInteger" (utils/parse-shorthand-value o) (XSD_NS)))
              ((index-of? '("Decimal") o-type) (format "\"~a\"^^~a:decimal" (utils/parse-shorthand-value o) (XSD_NS)))
              ((index-of? '("Float") o-type) (format "\"~a\"^^~a:float" o (XSD_NS)))
              ((index-of? '("Double") o-type) (format "\"~a\"^^~a:double" o (XSD_NS)))
              ((index-of? '("String") o-type) (format "\"~a\"^^~a:string" o (XSD_NS)))
              ((index-of? '("Year") o-type) (format "\"~a\"^^~a:gYear" o (XSD_NS)))
              ((index-of? '("Date") o-type) (format "\"~a-~a-~a\"^^~a:date"
                            (dexify-year (or (year o) "xxxx"))
                            (format-number "dd" (->number* (month o)) #:filler "0")
                            (format-number "dd" (->number* (day o)) #:filler "0")
                            (XSD_NS)))
              ((index-of? '("Class") o-type) (parse/to-rdf-name o))
              ((utils/literal-predicate? p (Tabtree)) (format "\"~a\"" o))
              (else (parse/to-rdf-name o)))))))))

(define (make-predicate-objects-str item)
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
                (-> p from-tabtree-alias parse/to-rdf-name)
                (object-value-str o p))))))
        (string-join " ;\n")
        (string-append " .")))))

(define-catch (clean-up-sections-refs tabtree)
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
        (old-ontology-id ($ __id ontology-item))
        (ontology-item (if (not-empty? imports)
                          (hash-union
                            ontology-item
                            (hash
                              "__id" (hash-ref ontology-item "ns" "ontology")
                              "owl/imports" imports))
                          ontology-item))
        (ontology-item (hash-remove ontology-item "ns")))
    (hash-set tabtree old-ontology-id ontology-item)))

(define (tabtree->rdf tabtree)
  (Tabtree tabtree)
  (XSD_NS (->> tabtree
               hash-values
               (filter (λ (item) (equal? ($ __parent item) "namespaces")))
               (filter (λ (item) (equal? ($ ns item) XSD_NAMESPACE)))
               ($ __id)))
  (let* (
        (default-prefix (or
                          (some->>
                            tabtree
                            hash-values
                            (filter hash?)
                            (filter (λ (item) (equal? ($ a item) "owl/Ontology")))
                            only-or-first
                            ($ ns))
                          "https://example.org/ontology"))
        (tabtree (extract-imports tabtree))
        ; (_ (write-file "../capital-kgr/_temp/tabtree.rkt" tabtree))
        (namespaces (query/get-subtree '("namespaces") tabtree))
        (namespaces (hash-remove namespaces "namespaces"))
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
      (let* ((item (hash-ref tabtree id))
            (form (make-predicate-objects-str item)))
        (cond
          ((equal? (hash-ref item "a" #f) "owl/Ontology")
            (format "~a<~a>\n~a\n\n" res ($ __id item) form))
          (form
            (format "~a~a~a\n~a\n\n" res (or (parse/namespace ($ __id item)) "") (parse/to-rdf-name (parse/baseid ($ __id item))) form))
          (else res))))))
