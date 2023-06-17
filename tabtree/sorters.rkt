#lang racket

(require odysseus)
(require "parse.rkt")
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string))
(require (file "~/.local/share/tabtree/settings.rkt"))

(provide (all-defined-out))

(define (get-extended-value item key (default "0"))
  (define (extend-value value)
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
          (value (string-replace value #px"[a-zа-я]" "")))
      (->number value)))
  (let* ((value (hash-ref item key default)))
    (if (list? value)
      (->> value (map extend-value) (apply max))
      (extend-value value))))

(define (extend-numeric key)
  (λ (item)
    (get-extended-value item key "0")))
