#lang racket

;; in this file we represent functions that modify elements of the tree

(require odysseus)
(require "parse.rkt")
(require "globals.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string))

(provide (all-defined-out))

(define-catch (deidify s)
  (-> s string-titlecase (string-replace "_" " ")))

(define-catch (id> item-a item-b)
  (let ((id-a ($ __id item-a))
        (id-b ($ __id item-b)))
    (and
      id-a
      id-b
      (id-string>? id-a id-b))))

(define (id< item-a item-b)
  (not (id> item-a item-b)))

(define (id* item)
  ; extract both id and alternative id (alt) from item
  (flatten `(,(id item) ,($ alt item))))

(define-catch (filter-tabtree f tabtree)
  (for/fold
    ((res (hash)))
    (((id item) tabtree))
    (if (f id item)
      (hash-union
        res
        (hash id item))
      res)))

(define-catch (map-tabtree f tabtree)
  (for/fold
    ((res (hash)))
    (((id item) tabtree))
    (hash-union
      res
      (hash
        id
        (f item)))))

(define-catch (filter-map-tabtree f tabtree)
  (for/fold
    ((res1 (hash)))
    (((id item) tabtree))
    (hash-union
      res1
      (hash
        id
        (for/fold
          ((res2 (hash)))
          (((k v) item))
          (if (f v)
            (hash-union res2 (hash k (f v)))
            res2))))))

(define-catch (remove-specials-extended m)
  (define (reserved-predicate? s)
    (index-of? reserved-predicates (deprefix s)))
  (define (source-key? k)
    (equal? k "__source"))
  (for/fold
    ((res (hash)))
    (((k v) m))
    (cond
      ((source-key? k) (hash-set res k v))
      ((special? k) res)
      ((reserved-predicate? k) res)
      (else
        (let* ((deprefixed-k (deprefix k))
              (old-v (hash-ref m deprefixed-k #f)))
          (if old-v
            (hash-set res deprefixed-k old-v)
            (hash-set res deprefixed-k v)))))))

(define (get-children-ids tabtree)
  (->>
    tabtree
    hash-values
    (map (λ (item) ($ __children item)))
    flatten
    remove-duplicates
    (filter-not false?)))

(define (get-upper-level-ids tabtree)
  (let* ((all-ids (hash-keys tabtree))
        (children-ids (get-children-ids tabtree))
        (upper-level-ids (minus all-ids children-ids)))
    upper-level-ids))

(define-catch (special? s)
  (string-prefix? s "__"))

(define-catch (reification? s)
  (index-of? '("rdf/subject" "rdf/predicate" "rdf/object") s))

(define-catch (type? s)
  (index-of? '("a" "+a" "*a" "type-of" "instance-of" "eq" "eq-property" "owl/sameAs") s))

(define-catch (remove-specials item)
  (hash-filter (λ (k v) (not (special? k))) item))

(define-catch (remove-reifications item)
  (hash-filter (λ (k v) (not (reification? k))) item))

(define-catch (remove-types item)
  (hash-filter (λ (k v) (not (type? k))) item))

(define (statement? item)
  (equal? ($ a item) "rdf/Statement"))

; merge two tabtrees
(define (t+ . tabtrees)
  ; TODO make more sane merge
  (apply hash-union tabtrees))

(define (shorthand-value? o)
  (->> o string-downcase (regexp-match? #px"^[<>~]?\\d+[kкmмgt]$")))

(define (parse-shorthand-value value)
  (let* ((value (string-downcase value))
        (value (string-replace value #px"[<>~]" "")) ; TODO: somehow to manage this information?
        (dollars? (string-suffix? value "$"))
        (value (string-replace value "$" ""))
        (suffix (string-last value))
        (k (case suffix
              (("k" "к") 1000)
              (("m" "м") 1000000)
              (("g") 1000000000)
              (("t") 1000000000000)
              (else 1)))
        (value (string-replace value #px"[a-zа-я]" ""))
        (value (->number value))
        (value (* value k))
        (value (exact-floor value)))
    value))

(define-catch (literal-predicate? predicate tabtree)
  (let ((predicate (hash-ref aliases predicate predicate)))
    (cond
      ((list? predicate) (ormap (curryr literal-predicate? tabtree) predicate))
      ((not predicate) #f)
      ((index-of? '("rdfs/comment" "rdfs/label" "ns") predicate) #t)
      (else
        (let* ((predicate-item (hash-ref tabtree predicate (hash)))
              (predicate-range (hash-ref predicate-item "range" #f))
              (predicate-superproperty (hash-ref predicate-item "subproperty-of" #f)))
          (cond
            ((and predicate-range (literal? predicate-range tabtree))
              #t)
            (else
              (and predicate-superproperty (literal-predicate? predicate-superproperty tabtree)))))))))

(define-catch (literal? class tabtree)
  ; (--- 333 class)
  (cond
    ((list? class) (ormap (curryr literal? tabtree) class))
    ((not class) #f)
    ((index-of? section-ids class) #f)
    ((index-of? (list "owl/Thing") class) #f)
    ((index-of? (list "Year" "Date" "String" "Double" "Float" "Decimal" "PositiveInteger" "Integer" "Boolean") class) #t)
    ((index-of? (list "rdfs/Literal") class) #t)
    (else
      (let* ((class-item (hash-ref tabtree class (hash)))
            (superclass (hash-ref class-item "subclass-of" #f)))
        ; (--- 444 class superclass)
        (literal? superclass tabtree)))))
