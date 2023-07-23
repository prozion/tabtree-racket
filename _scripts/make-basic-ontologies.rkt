#lang racket

(require odysseus)
(require tabtree/parse)

(define basic_ontologies
  (hash-union
    (parse-tabtree "../ontologies/rdf.tree" #:namespace "rdf")
    (parse-tabtree "../ontologies/rdfs.tree" #:namespace "rdfs")
    (parse-tabtree "../ontologies/owl.tree" #:namespace "owl")
    ))

(write-file "../tabtree/basic-ontologies.rkt" (format "#lang racket\n(provide (all-defined-out))\n(define BASIC_ONTOLOGIES ~s)" basic_ontologies))
