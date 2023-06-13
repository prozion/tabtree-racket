;; functions from output.clj

#lang racket

(require odysseus)
(require "utils.rkt")
(require "parse.rkt")
(require "rdf.rkt")

(provide (all-defined-out))

;;; CSV
(define (make-csv tabtree #:delimeter (delimeter "\t") #:headers (headers #f) #:collection-delimeter (collection-delimeter ","))
  (let* ((tabtree-cleaned (hash-map (λ (k v) (values k (remove-specials-extended v))) tabtree))
        (headers (or
                    headers
                    (cons "id" (->> tabtree-cleaned hash-values (map hash-keys) flatten remove-duplicates))))
        (header-str (implode headers delimeter))
        (csv (for/fold
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
                                      h-val))
                            (h-val (string-replace h-val "\"" "")))
                        (pushr res2 h-val)))
                    delimeter))))
        (csv (string-append csv "\n")))
    csv))

;;; Tabtree string
(define (tabtree->string tabtree #:sorter (sorter id>) #:pars-print-order (pars-print-order #f))
  (define (add-meta object predicate subject)
    (let* ((target-statement (->> tabtree
                                  hash-values
                                  (filter statement?)
                                  (filter
                                    (λ (item)
                                      (and
                                        (equal? object ($ rdf/object item))
                                        (equal? predicate ($ rdf/predicate item))
                                        (equal? subject ($ rdf/subject item)))))
                                  first-or-only)))
      (if-not target-statement
        object
        (let ((target-statement (->> target-statement remove-specials remove-reifications remove-types)))
          (format
            "~a~a"
            object
            (for/fold
              ((res ""))
              (((k v) target-statement))
              (format "~a^~a:~a" res k v)))))))
  (define (process-val val predicate subject)
    (cond
      ((rdf-list? val)
        (let* ((vals (hash-ref val "__values" empty))
              (vals (string-join vals ",")))
          (format "`~a`" vals)))
      ((list? val)
        (let ((val (map (λ (v) (add-meta v predicate subject)) val)))
          (string-join val ",")))
      (else (add-meta val predicate subject))))
  (define (item->string item (ks-order-sequence #f))
    (let* ((ks (-> item remove-specials-extended hash-keys))
          (ks (if ks-order-sequence
                (sort-by-order ks ks-order-sequence)
                ks)))
      (for/fold
        ((res ($ __id item)))
        ((k ks))
        (let ((v (or
                    (hash-ref item k #f)
                    (hash-ref item (inheritance+ k) #f))))
          (format "~a ~a:~a" res k (process-val v k ($ __id item)))))))
  (define (tabtree->string-recur item-ids tablevel)
    (let* ((items (map (λ (item-id) (hash-ref tabtree item-id #f)) item-ids))
          (items (filter-not statement? items))
          (items (cleanmap items))
          (sorted-items (sort items sorter)))
      (for/fold
        ((res ""))
        ((item sorted-items))
        (let* ((children-ids ($ __children item))
              (pars-print-order
                (and
                  pars-print-order
                  (string-split pars-print-order ","))))
          (format
            "~a~a~a\n~a"
            res
            (dupstr "\t" tablevel)
            (item->string item pars-print-order)
            (if children-ids
              (tabtree->string-recur children-ids (+ 1 tablevel))
              ""))))))
  (tabtree->string-recur
    (get-upper-level-ids tabtree)
    0))


; (define (tabtree->string tabtree #:sorter (sorter classical-sorter) #:max-chunk-size (max-chunk-size #f) #:pars-print-order (pars-print-order))
;   #t)
