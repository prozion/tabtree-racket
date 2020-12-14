#lang racket

(require odysseus)

(provide (except-out (all-defined-out) category? vanilla-category?))

;;;;; For storing data from tabtree format we will use a following model:
;;;;; (hash a (hash) b (hash) c (hash c1 (hash) c2 (hash c21 (hash))))
;;;;; We build a nested hash structure (hashtree), where every element is a hash key.
;;;;; For manipulation with hashtrees we use functions from hashtree.rkt

(define mtree (make-parameter #f))

(define-catch (get-item-name item)
  (let ((name (hash-ref item 'name #f))
        (id (hash-ref item key-name #f)))
    (cond
      (name #f)
      ((hash-empty? item) #f)
      ((not id) #f)
      ((empty? id) #f)
      (else (string-replace (->string id) "_" " ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; category (one that starts with a small letter)
(define-catch (category? line)
  (re-matches? "^\t*[_a-zа-я.][A-Za-zА-Яа-яё0-9_\\-+/]+\\s*.*$" line))

; just a category without parameters in a mtree variant
(define-catch (vanilla-category? line)
  (re-matches? "^\t*[A-Za-zА-ЯЁа-яё0-9\\-+/_]+\\s*\\.$" line))

;;; Reads one line from a tab-tree file into a hash.
;;; Line has a following format:
;;; <name> <parameter-name>:<parameter-value> ... <string-parameter-name>:"<string-parameter-value>" ...
;;;
;;; The resulting hash is (hash <key-name> <name> <parameter-name> <parameter-value> ... <string-parameter-name> "<string-parameter-value>" ...)
;;; This hash is identified by <key-name> special attribute. Underscore in the beginning of key name is reserved symbol in this system of data representation.
;;; All keys that starts with _ has a special meaning.
(define-catch (get-item line)
  (let* (
        ; read <name>
        (res-name (if (mtree)
                    ; if parsing in mtree mode:
                    (get-matches #px"^\t*([\"λ#A-Za-zА-ЯЁа-яё0-9&@\\-_+/|<>\\?!]+)" line)
                    ; if parsing in tree mode, include dot:
                    (get-matches #px"^\t*([\"λ#A-Za-zА-ЯЁа-яё0-9&@.\\-_+/|<>\\?!]+)" line)))
        ; read all parameters, that have a string value
        (res-string-parameters
              (get-matches #px"(\\S+?):[\"`]([^\"`]+?)[\"`]" line))
        ; read all parameters, that have value as reference, date, number, special value etc. except string
        (res-parameters
              (get-matches #px"(\\S+?):(\\S+)" line))
        ; accumulate parameters to a hash
				(parameters (for/fold
                      ((res (hash)))
                      ((p res-parameters))
                      (hash-insert-fuse res (cons (->symbol (list-ref p 1)) (->symbol (list-ref p 2))))))
        ; in the case of `<code>` there can be 'name:<..>' pattern inside, it should not be evaluated to name as item parameter
        (parameters (hash-delete parameters 'name))
        ; accumulate string parameters to a hash
				(string-parameters (for/fold
                              ((res (hash)))
                              ((p res-string-parameters))
                              (hash-insert-fuse res (cons (->symbol (list-ref p 1)) (list-ref p 2)))))
        ; join two hashes
        (res (hash-union string-parameters parameters))
        ; add id to item
        (res (if res-name
            (hash-insert res (cons key-name (nth (nth res-name 1) 2))) ; if the same key comes several times in item's definition, its values will be fused
            res))
        ; add name to item, if it is not already defined:
        (name (get-item-name res))
        (res (if name
                  (hash-insert res (cons '_name name))
                  res))
        )
    res))

; Iteratively populates nested list structure (tree) with cons-hashes (item)
(define-catch (fill-tree-iter source-lines result-tree curpath (n 0) #:tags (tags empty))
  (define (clean-tags tagcons current-level)
    (filter-not (λ (x) (>= (car x) current-level)) tagcons))
  (define (tagcons->string tagcons)
    (implode (map cdr tagcons)))
  (cond
    ; if no more lines then exit the iteration and return result-tree
    ((empty? source-lines) result-tree)
    ; if some more source-lines...
    (else
      (let* ((line (car source-lines)) ; take the next source-line, now it is a "current line"
            (line (first (string-split line ";"))) ; take content just before comments
            ; (_ (--- line))
            (level (count-tabs line))
            (delta-level (- level (dec (length curpath)))) ; find the level of the current line
            (item (get-item line)) ; parse current line to item
            ; get all +parameters in the current line
            ; convert to string all general (not special) parameters:
            (item (map-hash
                    (λ (k v) (if (and k (special-parameter? k))
                                (values k v)
                                (values k (->string v))))
                    item))
            ; direct order (a1 a2 a3), reverse order (a3 a2 a1)
            ; parents - reverse order
            (parents (cond
                        ((> delta-level 0)
                            curpath) ; take current last element in the path as parent -> go down one level
                        ((= delta-level 0)
                            (cdr curpath)) ; take the parent of last element as a parent, last element becomes a neighbour to the new item
                        ((< delta-level 0)
                            (triml curpath (+ 1 (abs delta-level))))))
            (nextpath (cons (get-key item) parents))
            (item (if (empty? parents)
                      (add-parent item #f) ; root element has no parents
                      (add-parent item (car parents)))) ; otherwise, a parent is the last element in the path sequence
            ; add tags
            (tags (clean-tags tags level))
            (tag ($ t item))
            (tags (if tag (cons (cons level tag) tags) tags))
            (tags (uniques tags))
            ; (_ (when ($ t item) (--- tags ($ id item))))
            (item (if (not-empty? tags)
                      (hash-union (hash '_t (tagcons->string tags)) item)
                      item))
            ; add label to item
            (item (hash-union (hash '_order n '_label (if (empty? curpath) "" (last curpath))) item))
            ; add item to the result-tree:
            (new-result-tree (cond
                                ; if empty hash, then don't do anything with result-tree
                                ((hash-empty? item) result-tree)
                                ; otherwise add item to the end of parents sequence
                                (else (hash-tree-add-value-by-id-path* result-tree (reverse parents) item key-name)))))
        (fill-tree-iter (cdr source-lines) new-result-tree nextpath (+ 1 n) #:tags tags))))) ; repeat the process for the rest of source lines

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
(define-catch (parse-tab-tree tree-file)
  (parameterize ((mtree #f))
    (let* (
          ;; take all lines from the file:
          (tree-lines (read-file-by-lines tree-file))
          ;; remove comment lines:
          (tree-lines (clean
                        (λ (line) (or
                                    (re-matches? "^\t*;" line)
                                    (re-matches? "^\\s*$" line)))
                        tree-lines)))
        ;; start populating the tree
        (add-inherities
          (fill-tree-iter tree-lines (hash) empty)))))

(define parse-tabtree parse-tab-tree)

(define-catch (parse-tab-mtree tree-file)
  (parameterize ((mtree #t))
    (let* (
          ;; take all lines from the file:
          (tree-lines (read-file-by-lines tree-file))
          ;; remove comment lines:
          (tree-lines (clean
                        (λ (line) (or
                                    (re-matches? "^\t*;" line)
                                    (re-matches? "^\\s*$" line)))
                        tree-lines))
          ;; group lines
          (tree-lines (let loop ((res (list)) (endline? #f) (rest-lines tree-lines))
                        (cond
                          ((empty? rest-lines) res)
                          (else
                              (let ((res (if endline?
                                            (pushr res (list (car rest-lines)))
                                            (let* (
                                                  ; get the last group and insert there a new line
                                                  (last-group (if (not-empty? res) (last res) (list)))
                                                  (last-group (pushr last-group (car rest-lines)))
                                                  (res (trimr res 1))
                                                  (res (pushr res last-group)))
                                              res))))
                                ; (--- (car rest-lines) (vanilla-category? (car rest-lines)))
                                (loop
                                  res
                                  (if (or
                                        ; just a category without parameters
                                        (vanilla-category? (car rest-lines))
                                        ; end expression
                                        (re-matches? ".*<\\.>\\s*$" (car rest-lines)))
                                      #t #f)
                                  (cdr rest-lines)))))))
          (tree-lines (map
                        (λ (group)
                          (let* (
                                  ; clear <.>
                                  (last-line (last group))
                                  (last-line (string-replace last-line #rx"<\\.>\\s*" ""))
                                  (group (pushr (trimr group 1) last-line))
                                  ; remove extra tabs in extra lines
                                  (parameter-lines (cdr group))
                                  (parameter-lines (if (not-empty? parameter-lines)
                                                      (map
                                                        (λ (line) (string-replace ; handle sbgn-lisp lines
                                                                      (string-replace line #rx"[\t\n]+" "") ; remove leading tabs
                                                                      "`" "\""))
                                                        parameter-lines)
                                                      parameter-lines))
                                  (line (str (first group) " " (implode parameter-lines " "))))
                            line))
                        tree-lines)))
      (fill-tree-iter tree-lines (hash) empty))))

(define-catch (extract-tabtree-frame input-file output-file)
  (let* (
        ;; take all lines from the file:
        (tree-lines (read-file-by-lines input-file))
        ;; remove comment lines:
        (tree-lines (clean
                      (λ (line) (or
                                  (re-matches? "^\t*;" line)
                                  (re-matches? "^\\s*$" line)))
                      tree-lines))
        ;; leave just categories:
        (tree-lines (filter category? tree-lines))
        )
    (write-file-by-lines output-file tree-lines))
    #t)
