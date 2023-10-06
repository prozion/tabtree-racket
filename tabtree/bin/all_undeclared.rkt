#!/usr/bin/env racket

#lang racket

(require odysseus)
(require odysseus/cmdline)
(require tabtree)
(require tabtree/utils)
(require tabtree/globals)

(define literal-predicates (make-parameter (hash)))
(define tabtree-file (get-command-value (current-command-line-arguments)))
(define tabtree (parse-tabtree tabtree-file))

(define-catch (find-undeclared-predicates tabtree)
  (let* (
        (all-keys (->> tabtree hash-values (map remove-specials) (map hash-keys) flatten remove-duplicates (filter-not namespaced?)))
        (special-ids (join (hash-keys aliases) reserved-predicates))
        (declared-ids (-> tabtree hash-keys)))
    (minus all-keys (join declared-ids special-ids))))

(define-catch (find-undeclared-objects tabtree (id-likes empty))
  (let* ((all-p-o (->> tabtree hash-values (map remove-specials) (apply hash-union-merge)))
        (not-literal-p-o
          (for/fold
            ((res (hash)))
            (((k vs) all-p-o))
            (let ((cached-value (hash-ref (literal-predicates) k #f)))
              (cond
                ((index-of? (list "alt" "rdf/subject" "rdf/predicate" "rdf/object") k) res)
                ((index-of? id-likes k) res)
                ((equal? cached-value "literal") res)
                ((equal? cached-value "non-literal")
                  (hash-union res (hash k vs)))
                ((literal-predicate? k tabtree)
                  (literal-predicates (hash-union (literal-predicates) (hash k "literal")))
                  res)
                (else
                  (literal-predicates (hash-union (literal-predicates) (hash k "non-literal")))
                  (hash-union res (hash k vs)))))))
        (not-literal-os (->> not-literal-p-o hash-values flatten remove-duplicates))
        (declared-ids (-> tabtree hash-keys))
        (alias-ids (hash-keys aliases))
        (namespaced-ids (filter namespaced? not-literal-os)))
    (minus (minus not-literal-os (join namespaced-ids alias-ids)) declared-ids)))

(--- "Undefined predicates:" (list->pretty-string (find-undeclared-predicates tabtree)))
(--- "Undefined objects:" (list->pretty-string (find-undeclared-objects tabtree (list "sid" "tsid"))))
