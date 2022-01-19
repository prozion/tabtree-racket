#lang racket

(require odysseus)

(provide (except-out (all-defined-out) category?))

;;;;; Tabtree2 uses a simplier model for
;; a
;; b
;; c
;;    c1
;;    c2
;;        c21:
;
;; (hash 'a (hash) 'b (hash) 'c (hash '__children '(c1 c2))
;;       'c1 (hash))
;;       'c2 (hash '__children '(c21))
;;       'c21 (hash))

(define ns (make-parameter #f))

(define-catch (strip-ns s)
  (let ((astr (->string s)))
    (if (indexof? astr "/")
      (->symbol (string-join (rest (string-split astr "/"))))
      s)))

(define-catch (id item)
  (let ((full-id ($ __id item)))
    (if (ns)
      (let ((local-id (strip-ns full-id)))
        local-id)
      full-id)))

(define-catch (category? line)
  (re-matches? "^\t*[_a-zа-я.][A-Za-zА-Яа-яё0-9_\\-+/]+\\s*.*$" line))

(define (namespaced? chunk) (indexof? (->string chunk) "/"))

(define-catch (->symbol-ns chunk)
  (cond
    ((and (ns) (not (namespaced? chunk))) (->symbol (format "~a/~a" (ns) (->string chunk))))
    (else (->symbol chunk))))

(define-catch (get-item line)
  (let* (
        ; read <name>
        (res-name (get-matches #px"^\t*([\"λ#*A-Za-zА-ЯЁа-яё0-9&@:.\\-_+/|<>\\?!]+)" line))
        ; read all parameters, that have a string value
        (res-string-parameters
              (get-matches #px"(\\S+?):[\"`]([^\"`]+?)[\"`]" line))
        (res-number-parameters
              (get-matches #px"(\\S+?):([-+.0-9]+)" line))
        (res-multiple-parameters
              (get-matches #px"(\\S+?):(\\S*,\\S*)" line))
        ; read all parameters, that have value as reference, date, special value etc. except string and number
        (res-parameters
              (get-matches #px"(\\S+?):(\\S+)" line))
        ; accumulate parameters to a hash
        (parameters (for/hash
                      ((p `[,@res-parameters ,@res-string-parameters ,@res-number-parameters ,@res-multiple-parameters]))
                      (match-let (((list _ p1 p2) p))
                        (values (->symbol-ns p1)
                                (cond
                                  ((indexof? res-string-parameters p) p2)
                                  ((indexof? res-number-parameters p) (->number p2))
                                  ((indexof? res-multiple-parameters p) (map ->symbol-ns (string-split p2 ",")))
                                  (else (->symbol-ns p2)))))))
        ; add id to item
        (res (match-let ((`((,_ ,id)) res-name))
                (if id
                    (hash (->symbol-ns id) (hash-union parameters (hash '__id (->symbol-ns id))))
                    parameters)))
        )
    res))

(define (get-item-parameter key node tabtree-h)
  (let ((inherited-key (inheritance+ key)))
    (or
      (hash-ref* (hash-ref* tabtree-h node (hash)) key #f)
      (hash-ref* (hash-ref* tabtree-h node (hash)) inherited-key #f))))

(define-catch (get-parameter path tabtree-h)
  (let* ((reversed-path (reverse path))
        (parameter-key (->symbol-ns (first reversed-path)))
        (node (->symbol-ns (second reversed-path))))
    (get-item-parameter parameter-key node tabtree-h)))

(define-catch (nodes-path? path tabtree-h)
  (true? (hash-ref* tabtree-h (->symbol-ns (last path)) #f)))

(define-catch (item+ item new-parameters-h)
  (match-let (((hash-table (id old-parameters-h)) item))
    (hash id (hash-union old-parameters-h new-parameters-h))))

(define-catch (item-parameters item)
  (first (hash-values item)))

(define (inherity? key)
  (re-matches? "\\+[\\-a-z0-9]+" (->string key)))

(define (inheritance+ key)
  (->symbol-ns (str "+" (->string (strip-ns key)))))

(define-catch (get-inherities parameters-h)
  (hash-filter
    (λ (k v) (inherity? (strip-ns k)))
    parameters-h))

(define-catch (fill-tree-iter2 source-lines #:inherities (inherities (hash)))
  (let* ((root-line (first source-lines))
        (next-lines (rest source-lines))
        (root-tabs-count (count-tabs root-line))
        (root-item (get-item root-line))
        (global-inherities inherities)
        (local-inherities (get-inherities (item-parameters root-item)))
        (inherities (hash-union local-inherities global-inherities))
        (_ (split-with (λ (line)
                            ; (--- line (count-tabs line) root-tabs-count)
                            (> (count-tabs line) root-tabs-count))
                        next-lines))
        (sublines (first _))
        (next-block (second _))
        (top-sublines (filter
                        (λ (line) (= (count-tabs line) (inc root-tabs-count)))
                        sublines))
        (children-ids (map (--> ->symbol-ns id hash-values get-item) top-sublines))
        (root-item (if (not-empty? children-ids)
                      (item+ root-item (hash '__children children-ids))
                      root-item))
        (root-item (item+ root-item inherities))
        (result-hash (if (empty? sublines)
                        root-item
                        (hash-union root-item (fill-tree-iter2 sublines #:inherities inherities))))
        )
    (cond
      ((empty? next-block) result-hash)
      (else (hash-union result-hash (fill-tree-iter2 next-block #:inherities global-inherities))))))

(define-catch (add-inherities tab-tree #:inherities (inherities (hash)))
  (map-hash
    (λ (k v)
      (let* ((local-inherities (hash-filter
                                  (λ (k1 v1) (re-matches? "\\+[\\-a-z0-9]+" (->string k1)))
                                  k))
            (inherities (hash-union local-inherities inherities)))
        (values
          (hash-union inherities k)
          (add-inherities v #:inherities inherities))))
    tab-tree))

; Initialize population of a tree with data from tabtree file
(define-catch (parse-tab-tree tree-file #:namespace (namespace #f))
  (parameterize ((ns namespace))
    (let* (
          ;; take all lines from the file:
          (tree-lines (read-file-by-lines tree-file))
          ;; remove comment lines:
          (tree-lines (clean
                        (λ (line) (or
                                    (re-matches? "^\t*;" line)
                                    (re-matches? "^\\s*$" line)))
                        tree-lines)))
        (fill-tree-iter2 tree-lines))))
        ;; start populating the tree
        ; (add-inherities
        ;   (fill-tree-iter tree-lines (hash) empty))))

(define parse-tabtree parse-tab-tree)
