#lang racket

;; in this file we represent functions that modify elements of the tree

(require odysseus)
(require "tabtree.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list))

(provide (all-defined-out))

(define-catch (get-subtree path tabtree-h)
  (define (collect-children next-children (checked-children empty))
    (cond
      ((empty? next-children) checked-children)
      (else
        (let* ((next-child (first next-children))
              (checked-children (pushr checked-children next-child))
              (new-children (or ($ __children (hash-ref* tabtree-h next-child (hash))) empty))
              (new-next-children (remove-duplicates (append (rest next-children) (minus new-children checked-children)))))
          (collect-children new-next-children checked-children)))))
  (let ((children-ids (collect-children (list (last path)))))
    (hash-filter (λ (k _)  (indexof? children-ids k)) tabtree-h)))

(define-macro ($t path tabtree-h . ns)
    (let ((path-as-list (symbol-split path "."))
          (namespace (if (empty? ns) #f (first ns))))
      `(parameterize ((ns ,namespace))
        (cond
          ((nodes-path? '(,@path-as-list) ,tabtree-h) (get-subtree '(,@path-as-list) ,tabtree-h))
          (else (get-parameter '(,@path-as-list) ,tabtree-h))))))
