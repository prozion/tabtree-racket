#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require "../main.rkt")

(define filepath (get-command-value (current-command-line-arguments)))
(define options (get-command-options (current-command-line-arguments)))

(define (check-duplicated-ids)
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* (
        (duplicated-ids ($ duplicated-ids tabtree)))
    (displayln "Duplicated ids [lines]:")
    (for (((k v) duplicated-ids))
      (displayln (format "~a [~a]" k (implode v ", "))))))

(define (check-duplicated-lines)
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* ((duplicated-ids ($ duplicated-ids tabtree))
         (duplines (hash-filter (λ (k v) (check-duplicates v)) duplicated-ids)))
     (displayln "Duplicated lines [one of the lines]:")
     (for (((k v) duplines))
       (displayln (format "~a [~a]" k (check-duplicates v))))))

(case (hash-ref options "--check" #f)
  (("dupids")
    (check-duplicated-ids))
  (("duplines")
    (check-duplicated-lines))
  (else (--- "Usage: tabtree-checks.rkt --check (dupids|duplines) TABTREE_FILEPATH\nDon't try 'man tabtree-checks' as it is not written yet.")))
