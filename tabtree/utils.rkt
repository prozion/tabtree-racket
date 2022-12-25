#lang racket

;; in this file we represent functions that modify elements of the tree

(require odysseus)
(require "tabtree.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string))

(provide (all-defined-out))

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
