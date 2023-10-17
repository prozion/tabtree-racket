#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require "../main.rkt")

(define filepath (get-command-value (current-command-line-arguments)))
(define options (get-command-options (current-command-line-arguments)))

(define (get-lines h)
  (->> h hash-values flatten (map ->int) remove-duplicates ((curryr sort <))))

(define (check-duplicated-ids #:print-count (count? #f) #:print-only-lines (print-only-lines-sep #f))
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* (
        (duplicated-ids ($ duplicated-ids tabtree)))
    (displayln (format "Duplicated ids [lines]~a:" (if count? (format " (Total ~a)" (length (hash-keys duplicated-ids))) "")))
    (if print-only-lines?
      (displayln (list->pretty-string (get-lines duplicated-ids) print-only-lines-sep))
      (for (((k v) duplicated-ids))
        (displayln (format "~a [~a]" k (implode v ", ")))))))

(define (check-duplicated-lines #:print-count (count? #f) #:print-only-lines (print-only-lines-sep #f))
  (define tabtree (parse-tabtree filepath #:parse-info #t))
  (let* ((duplicated-ids ($ duplicated-ids tabtree))
         (duplines (hash-filter (λ (k v) (check-duplicates v)) duplicated-ids)))
     (displayln (format "Duplicated lines [one of the lines]~a:" (if count? (format " (Total ~a)" (length (hash-keys duplines))) "")))
     (if print-only-lines?
       (displayln (list->pretty-string (get-lines duplines) print-only-lines-sep))
       (for (((k v) duplines))
         (displayln (format "~a [~a]" k (check-duplicates v)))))))

(define print-count? (hash-ref options "--print-count" #f))
(define print-only-lines? (hash-ref options "--print-only-lines" #f))
(define option (hash-ref options "--option" #f))

(case (hash-ref options "--option" #f)
  (("dupids")
    (check-duplicated-ids #:print-count print-count? #:print-only-lines print-only-lines?))
  (("duplines")
    (check-duplicated-lines #:print-count print-count? #:print-only-lines print-only-lines?))
  (else (--- "Usage: check.rkt --option (dupids|duplines) [--count true] [--print-only-lines \", \"] TABTREE_FILEPATH\n")))
