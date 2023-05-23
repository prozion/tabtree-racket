;; functions from output.clj

#lang racket

(require odysseus)
(require "utils.rkt")

(provide (all-defined-out))

(define (make-csv tabtree #:delimeter (delimeter "\t") #:headers (headers #f) #:collection-delimeter (collection-delimeter ","))
  (let* ((tabtree-cleaned (hash-map (λ (k v) (values k (remove-specials-extended v))) tabtree))
        (headers (or
                    headers
                    (cons "id" (->> tabtree-cleaned hash-values (map hash-keys) flatten remove-duplicates))))
        (header-str (implode headers delimeter)))
    (for/fold
      ((res1 header-str))
      (((k v) tabtree-cleaned))
      (string-append
        res1
        "\n"
        (implode
          (for/fold
            ((res2 empty))
            ((h headers))
            (let* ((h-val (if (equal? h "id")
                            k
                            (hash-ref v h "NA")))
                  (h-val (if (list? h-val)
                            (implode h-val collection-delimeter)
                            h-val)))
              (pushr res2 h-val)))
          delimeter)))))

; (define (tabtree->string tabtree #:sorter (sorter classical-sorter) #:max-chunk-size (max-chunk-size #f) #:parameters-sort-template (parameters-sort-template))
;   #t)
