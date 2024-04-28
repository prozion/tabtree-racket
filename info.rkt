#lang info

(define collection "tabtree")
(define deps '("base" "odysseus" "compatibility-lib"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-lib"))
(define scribblings '(("doc/tabtree.scrbl")))
(define pkg-desc "A library to handle files of Tabtree format")
