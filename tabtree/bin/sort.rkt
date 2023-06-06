#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require tabtree/output)
(require tabtree/utils)
(require "../main.rkt")

(define-namespace-anchor anchor)
(define ns-own (namespace-anchor->namespace anchor))

(define filepath (get-command-value (current-command-line-arguments)))
(define options (get-command-options (current-command-line-arguments)))

(define sort-mode ($ --by-id options))
(define help-command (equal? filepath "--help"))

(define pars-order ($ --pars-order options))

(define initial-tabtree
  (when (and filepath (not help-command))
    (parse-tabtree filepath)))

(cond
  (help-command
    (---
#<<str
Usage:
  1) ./sort.rkt --by-id (a-z|z-a) --pars-order [\"bar,foo,baz\"] TABTREE_FILEPATH
  2) ./sort.rkt --by-key [item-key] --pars-order [\"bar,foo,baz\"] TABTREE_FILEPATH
  3) ./sort.rkt --by-key [item-key] --order (a-z|z-a|0-9|9-0) --pars-order [\"bar,foo,baz\"] TABTREE_FILEPATH
  4) ./sort.rkt --by-f [(lambda (item) ...)] --pars-order [\"bar,foo,baz\"] TABTREE_FILEPATH
  5) ./sort.rkt --help
str
    ))
  ((equal? ($ --by-id options) "a-z")
    (write-file filepath (tabtree->string initial-tabtree #:sorter id< #:pars-print-order pars-order)))
  ((equal? ($ --by-id options) "z-a")
    (write-file filepath (tabtree->string initial-tabtree #:sorter id> #:pars-print-order pars-order)))
  (($ --by-key options)
    (let* ((key ($ --by-key options))
          (order (or ($ --order options) "a-z"))
          (comparator
            (case order
              (("a-z") id-string<?)
              (("z-a") id-string>?)
              (("0-9") (λ (a b) (< (->number a) (->number b))))
              (("9-0") (λ (a b) (> (->number a) (->number b))))
              (else string<?)))
          (get-first-val (λ (item)
                            (let ((val (hash-ref item key "")))
                              (if (list? val)
                                  (only-or-first (sort val comparator) "")
                                  val))))
          (comp-f (λ (item1 item2)
                    (let* ((val1 (get-first-val item1))
                          (val2 (get-first-val item2)))
                      (comparator val1 val2)))))
      (write-file filepath (tabtree->string initial-tabtree #:sorter comp-f #:pars-print-order pars-order))))
  (($ --by-f options)
    (let* ((f-str ($ --by-f options))
          (f (eval (read (open-input-string f-str)) ns-own))
          (comp-f (λ (item1 item2) (> (f item1) (f item2)))))
      (write-file filepath (tabtree->string initial-tabtree #:sorter comp-f #:pars-print-order pars-order))))
  (else (--- "Wrong command format")))

;;; Examples:
; ./sort.rkt --by-key share ~/projects/capital-kgr/people_test_sorting.tree
; ./sort.rkt --by-key f --order 9-0 --pars-order a,type,f,owner,co-owner,prime-share,share ~/projects/capital-kgr/people_test_sorting.tree
; ./sort.rkt --by-f '(lambda (item) (* (->number (or ($ f item) 0)) (string-length (or (only-or-first ($ share item)) ""))))' ~/projects/capital-kgr/people_test_sorting.tree
