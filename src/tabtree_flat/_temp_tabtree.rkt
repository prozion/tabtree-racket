; TODO ->
; (define-catch (get-parameter path tabtree)
;   (let* ((reversed-path (reverse path))
;         (parameter-key (->symbol-ns (first reversed-path)))
;         (node (->symbol-ns (second reversed-path))))
;     (get-item-parameter parameter-key node tabtree)))
;
; (define-catch (nodes-path? path tabtree-h)
;   (true? (hash-ref* tabtree-h (->symbol-ns (last path)) #f)))
;
; (define-catch (item+ item new-parameters-h)
;   (match-let (((hash-table (id old-parameters-h)) item))
;     (hash id (hash-union old-parameters-h new-parameters-h))))
;
; (define-catch (item-parameters item)
;   (first (hash-values item)))
;
; (define (inherity? key)
;   (re-matches? "\\+[\\-a-z0-9]+" (->string key)))
;
; (define-catch (get-inherities parameters-h)
;   (hash-filter
;     (λ (k v) (inherity? (basename k)))
;     parameters-h))

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

; ported from Clojure
(define-catch (get-subtree path tabtree)
  (let ((children-ids
          (let loop ((next-children (if (not-empty? path)
                                        (last path)
                                        empty))
                    (checked-children empty))
            (cond
              ((empty? next-children) checked-children)
              (else
                (let ((next-child (first next-children))
                          (checked-children (cons next-child checked-children))
                          (new-children (hash-ref
                                          (hash-ref tabtree next-child ITEM_NONE) "__children"
                                          empty))
                          (new-next-children
                            (remove-duplicates
                              (append
                                (rest next-children)
                                (minus new-children checked-children)))))
                  (loop
                    new-next-children
                    checked-children)))))))
      (for/fold
        ((acc (hash)))
        ((child-id children-ids))
        (hash-union acc (hash child-id (hash-ref tabtree child-id ITEM_NONE))))))

(define-catch (get-item line)
  (let* (
    ...
    (line-string-parameters
          (get-matches string-regexp line))
    (line-multiple-parameters
          (get-matches multiples-regexp line))
    ;;; read all parameters, that have value as reference, date, special value etc. except string and multiples
    (line-parameters
          (get-matches pair-regexp line))
    (all-line-parameters (append line-parameters line-multiple-parameters line-string-parameters))
    ;;; accumulate parameters to a hash
    (parameters (for/hash
                  ((p `[,@line-parameters ,@line-string-parameters ,@line-multiple-parameters]))
                  (match-let (((list _ key val) p))
                    (values (ns+ key)
                            (cond
                              ((indexof? line-string-parameters p) val)
                              ((indexof? line-multiple-parameters p) (map ns+ (string-split val ",")))
                              (else (ns+ val)))))))
                              )
...))
