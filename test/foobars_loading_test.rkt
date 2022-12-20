#lang racket

(require odysseus)
(require "../src/tabtree_flat/tabtree.rkt")

; (define foobars (parse-tabtree "fixtures/foobar.tree"))
(define foobars-namespaced (parse-tabtree "fixtures/foobar.tree" #:namespace "test"))

; (---- (map
;         (λ (item) (hash-ref item "__parent" NONE))
;         (hash-values foobars)))

(---- foobars-namespaced)
