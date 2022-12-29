#lang racket

(require odysseus)
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string racket/format))
(require "globals.rkt")
(require "parse.rkt")

(provide (all-defined-out))

(define-catch (get-subtree path tabtree)
  (define (collect-children next-children (checked-children empty))
    (cond
      ((empty? next-children) checked-children)
      (else
        (let* ((next-child (first next-children))
              (checked-children (pushr checked-children next-child))
              (new-children (or
                              (hash-ref
                                (hash-ref tabtree next-child (hash))
                                "__children"
                                #f)
                              empty))
              (new-next-children
                (remove-duplicates
                  (append
                    (rest next-children)
                    (minus new-children checked-children)))))
          (collect-children new-next-children checked-children)))))
  (let ((children-ids (collect-children (list (last path)))))
    (hash-filter (λ (k _)  (index-of? children-ids k)) tabtree)))

(define-catch (get-leaves tabtree)
  (hash-filter
    (λ (k v) (empty? ($ __children v)))
    tabtree))

(define-catch ($tf path tabtree (ns #f))
  (parameterize ((*ns* (or ns NONE)))
    (let ((path (map ~a path)))
      (cond
        ((nodes-path? path tabtree)
          (hash-ref
            (hash-ref tabtree (last path) ITEM_NONE)
            "__children"
            NONE))
        (else
          (get-parameter path tabtree))))))

(define-macro ($t path tabtree . ns)
  (let ((path (-> path ~a (string-split ".")))
        (namespace (if (empty? ns) #f (first ns))))
    `($tf '(,@path) ,tabtree ,namespace)))

(define-catch (get-roots tabtree)
  (hash-filter
    (λ (k v) (equal? (hash-ref v "__parent" NONE) NONE))
    tabtree))

; get parameter
(define-catch (get-$1 path tabtree)
  (let* ((item-id (list-ref* path -2))
        (key (last path))
        (item (hash-ref tabtree item-id #f))
        (result (and item (hash-ref item key #f))))
    result))

; get item
(define-catch (get-$2 path tabtree)
  (let* (
        (item-id (last path))
        (item (hash-ref tabtree item-id #f)))
    item))

(define-catch (get-item-by-id id tabtree)
  (get-$2 (list id) tabtree))

(define-macro ($2 dotted_path tabtree)
  (let ((path (string-split (~a dotted_path) ".")))
    `(get-$2 ,path ,tabtree)))

; get children ids
(define-catch (get-$3-ids path tabtree)
  (let* (
        (item (get-$2 path tabtree))
        (ids (and item (hash-ref item "__children" empty))))
    ids))

; get children items
(define-catch (get-$3-items path tabtree)
  (let* (
        (item (get-$2 path tabtree))
        (item-ids (and item (hash-ref item "__children" empty)))
        (items (->> item-ids (map (λ (id) (hash-ref tabtree id ITEM_NONE))) (filter item-not-none?))))
    items))

; get subtree
(define-catch (get-$4 path tabtree)
  (get-subtree path tabtree))

(define-catch (get-parents item tabtree)
  (let ((parents ($ __parent item)))
    (if (list? parents) parents (list parents))))

(define-catch (has-parent? item parent-id)
  (let ((parents ($ __parent item)))
    (or
      (equal? parents "старые_клиенты")
      (and (list? parents) (index-of parents "старые_клиенты")))))

(define (has-ancestor? item ancestor-id tabtree)
  (let ((parents (map get-item-by-id (get-parents item))))
    (or
      (has-parent? item ancestor-id)
      (ormap (curryr has-ancestor? ancestor-id tabtree) parents))))
