#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require "../main.rkt")

(define filepath (get-command-value (current-command-line-arguments)))
(define options (get-command-options (current-command-line-arguments)))

(define (check-duplicated-ids (count? #f))
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* (
        (duplicated-ids ($ duplicated-ids tabtree)))
    (displayln (format "Duplicated ids [lines]~a:" (if count? (format " (Total ~a)" (length (hash-keys duplicated-ids))) "")))
    (for (((k v) duplicated-ids))
      (displayln (format "~a [~a]" k (implode v ", "))))))

(define (check-duplicated-lines (count? #f))
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* ((duplicated-ids ($ duplicated-ids tabtree))
         (duplines (hash-filter (λ (k v) (check-duplicates v)) duplicated-ids)))
     (displayln (format "Duplicated lines [one of the lines]~a:" (if count? (format " (Total ~a)" (length (hash-keys duplines))) "")))
     (for (((k v) duplines))
       (displayln (format "~a [~a]" k (check-duplicates v))))))

(define print-count? (hash-ref options "--count" #f))

(case (hash-ref options "--option" #f)
  (("dupids")
    (check-duplicated-ids print-count?))
  (("duplines")
    (check-duplicated-lines print-count?))
  (else (--- "Usage: check.rkt --option (dupids|duplines) [--count true] TABTREE_FILEPATH\n")))
