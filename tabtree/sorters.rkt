#lang racket

(require odysseus)
(require "parse.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string))
; (require (file "~/.local/share/tabtree/settings.rkt"))
(require tabtree/utils)

(provide (all-defined-out))

(define (get-extended-value item key (default "0") #:f f)
  (let* ((value (hash-ref item key default)))
    (if (list? value)
      (->> value (map parse-shorthand-value) (apply f))
      (parse-shorthand-value value))))

(define (extend-numeric key (f first-element))
  (λ (item)
    (get-extended-value item key "0" #:f f)))
