#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require "../main.rkt")
(require "../utils.rkt")

(define filepath (get-command-value (current-command-line-arguments)))

(cond
  ((or
      (equal? filepath "--help")
      (equal? filepath ""))
    (--- "Usage: all-predicates.rkt [--output (line|column)] TABTREE_FILEPATH\n"))
  (else
    (define options (get-command-options (current-command-line-arguments)))
    (define output ($ --output options))
    (let* ((tabtree (parse-tabtree filepath))
          (keys (->> tabtree hash-values (map remove-specials) (map remove-reifications) (map remove-types) (map hash-keys) flatten remove-duplicates))
          (keys (sort keys a-z))
          (sep (if (equal? output "line") ", " "\n"))
          (keys-str (string-join keys sep)))
      (--- keys-str))))
