#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require tabtree/output)
(require tabtree/utils)
(require tabtree/sorters)
(require "../main.rkt")

(define-namespace-anchor anchor)
(define ns-own (namespace-anchor->namespace anchor))

(define src-path (get-command-value (current-command-line-arguments)))
(define options (get-command-options (current-command-line-arguments)))

(define sort-mode ($ --by-id options))
(define help-command (equal? src-path "--help"))

(define pars-order ($ --pars-order options))
(define ignore-keys (string-split (or ($ --ignore-keys options) "") ","))
(define dest-path ($ --dest options))

(define initial-tabtree
  (when (and src-path (not help-command))
    (parse-tabtree src-path)))

(cond
  (help-command
    (---
#<<str
Usage:
  1) ./sort.rkt --by-id (a-z|z-a) --pars-order [\"bar,foo,baz\"] --ignore-keys [\"bar.foo,baz\"] --dest [TABTREE_DEST_PATH] TABTREE_SRC_PATH
  2) ./sort.rkt --by-key [item-key] --pars-order [\"bar,foo,baz\"] --ignore-keys [\"bar.foo,baz\"] --dest [TABTREE_DEST_PATH] TABTREE_SRC_PATH
  3) ./sort.rkt --by-key [item-key] --order (a-z|z-a|0-9|9-0) --pars-order [\"bar,foo,baz\"] --ignore-keys [\"bar.foo,baz\"] --dest [TABTREE_DEST_PATH] TABTREE_SRC_PATH
  4) ./sort.rkt --by-f [(lambda (item) ...)] --pars-order [\"bar,foo,baz\"] --ignore-keys [\"bar.foo,baz\"] --dest [TABTREE_DEST_PATH] TABTREE_SRC_PATH
  5) ./sort.rkt --help
str
    ))
  ((equal? ($ --by-id options) "a-z")
    (write-file (or dest-path src-path) (tabtree->string initial-tabtree #:sorter id< #:pars-print-order pars-order #:ignore-keys ignore-keys)))
  ((equal? ($ --by-id options) "z-a")
    (write-file (or dest-path src-path) (tabtree->string initial-tabtree #:sorter id> #:pars-print-order pars-order #:ignore-keys ignore-keys)))
  (($ --by-key options)
    (let* ((keys (string-split ($ --by-key options) ","))
          (order (or ($ --order options) "a-z"))
          (comparator
            (case order
              (("a-z") (λ (a b) (id-string<? a b)))
              (("z-a") (λ (a b) (id-string>? a b)))
              (("0-9") (λ (a b) (< (->number a) (->number b))))
              (("9-0") (λ (a b) (> (->number a) (->number b))))
              (else string<?)))
          (get-first-val (λ (vals)
                              (if (list? vals)
                                  (only-or-first (sort vals comparator) "")
                                  vals)))
          (comp-f (λ (item1 item2)
                      (let loop ((vals1 (map get-first-val (hash-refs item1 keys "")))
                                (vals2 (map get-first-val (hash-refs item2 keys ""))))
                        (cond
                          ((empty? vals1) #t)
                          ((empty? vals2) #f)
                          ((comparator (first vals1) (first vals2)) #t)
                          ((comparator (first vals2) (first vals1)) #f)
                          (else (loop (rest vals1) (rest vals2))))))))
      (write-file (or dest-path src-path) (tabtree->string initial-tabtree #:sorter comp-f #:pars-print-order pars-order #:ignore-keys ignore-keys))))
  (($ --by-f options)
    (let* ((f-str ($ --by-f options))
          (f (eval (read (open-input-string f-str)) ns-own))
          (comp-f (λ (item1 item2) (> (f item1) (f item2)))))
      (write-file (or dest-path src-path) (tabtree->string initial-tabtree #:sorter comp-f #:pars-print-order pars-order #:ignore-keys ignore-keys))))
  (else (--- "Wrong command format")))

;;; Examples:
; ./sort.rkt --by-key share ~/projects/capital-kgr/people_test_sorting.tree
; ./sort.rkt --by-key f --order 9-0 --pars-order a,type,f,owner,co-owner,prime-share,share ~/projects/capital-kgr/people_test_sorting.tree
; ./sort.rkt --by-f '(lambda (item) (* (->number (or ($ f item) 0)) (string-length (or (only-or-first ($ share item)) ""))))' ~/projects/capital-kgr/people_test_sorting.tree
