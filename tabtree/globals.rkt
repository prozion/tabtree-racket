#lang racket

(provide (all-defined-out))

(define NONE "")
(define ITEM_NONE (hash))

(define HIERARCHY_RELATION "hi-rel")
(define HIERARCHY_INVERSE_RELATION "hi-inv-rel")

(define *ns* (make-parameter NONE))
