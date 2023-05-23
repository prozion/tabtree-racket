#lang racket

;; in this file we represent functions that modify elements of the tree

(require odysseus)
(require "parse.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string))

(provide (all-defined-out))

(define-catch (deidify s)
  (-> s string-titlecase (string-replace "_" " ")))

(define-catch (tabtree> a b)
  (cond
    ((empty-string? a) #f)
    ((empty-string? b) #t)
    (else
      (let* ((letters (string-explode "_абвгдеёжзийклмнопрстуфхцчшщьыъэюяabcdefghijklmnopqrstuvwxyz0123456789"))
            (a (string-downcase a))
            (b (string-downcase b))
            (a-first (string-first a))
            (a-rest (string-rest a))
            (b-first (string-first b))
            (b-rest (string-rest b))
            (a-pos (index-of letters a-first))
            (b-pos (index-of letters b-first)))
        (cond
          ((equal? a-first b-first) (tabtree> a-rest b-rest))
          (else (> a-pos b-pos)))))))

(define (tabtree< a b)
  (not (tabtree> a b)))

(define (filter-map-tabtree f tabtree)
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
    (index-of?
      '("hi-rel"
        "hi-inv-rel"
        "section-default-class")
      (deplus s)))
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
        (let* ((deplused-k (deplus k))
              (old-v (hash-ref m deplused-k #f)))
          (if old-v
            (hash-set res deplused-k old-v)
            (hash-set res deplused-k v)))))))
