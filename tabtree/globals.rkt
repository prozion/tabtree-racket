#lang racket

(provide (all-defined-out))

(define NONE "")
(define ITEM_NONE (hash))

(define HIERARCHY_RELATION "hi-rel")
(define HIERARCHY_INVERSE_RELATION "hi-inv-rel")

(define *ns* (make-parameter NONE))

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
    "def" "skos/definition"
    "deabbr" "dc/title"
    "name" "rdfs/label"
    "disjoint-with" "owl/disjointWith"
))
