#lang racket

(require rackunit)
(require odysseus)
(require "../main.rkt")
(require "../rdf.rkt")

; (define foobar (parse-tabtree "fixtures/foobar.tree"))
(define owl (parse-tabtree "../../../ontologies/tabtree/basic/owl.tree"))

(write-file "output/owl.ttl" (tabtree->rdf owl))
