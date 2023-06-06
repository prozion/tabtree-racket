#lang racket

(require odysseus)
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list racket/string racket/format))
(require "globals.rkt")

(provide deplus special? nodes-path? none? not-none? item-none? item-not-none? get-item get-parameter parse-tabtree namespace baseid namespaced? id inheritance+)

; (: ItemValue (U String (Listof String) (Mutable-HashTable String String)))
; (: Item (Mutable-HashTable String ItemValue))
; (: ItemId String)
; (: Tabtree (Mutable-HashTable ItemId Item))

;;;;; Flat tabtree uses a simplier model for
;; a
;; b
;; c
;;    c1
;;    c2
;;        c21:
;
;; (hash "a" (hash) "b" (hash) "c" (hash "__children" '("c1" "c2"))
;;       "c1" (hash))
;;       "c2" (hash "__children" '("c21"))
;;       "c21" (hash))

(define *parent-id* (make-parameter NONE))
(define *tabtree* (make-parameter (hash)))
(define *parse-info* (make-parameter (hash)))
(define *source-lines* (make-parameter empty))

(define id-regexp-str "[?_A-Za-zА-ЯЁа-яёΑ-Ωα-ωØÅøå0-9№\\*][\"#*A-Za-zА-ЯЁа-яёΑ-Ωα-ω0-9№ØÅøå&@:._?+/\\*|<>\\?!\\-]*")
(define id-regexp (pregexp id-regexp-str))
(define key-regexp-str "[_a-zA-ZА-ЯЁа-яёα-ω0-9/|+*\\-]+")
(define category-regexp "^\t*[_a-zа-яёα-ωøå.][A-Za-zА-Яа-яё0-9_+/\\-]+\\s*.*$")

(define (none? ns-or-id)
  (equal? ns-or-id NONE))

(define not-none? (negate none?))

(define (item-none? item)
  (equal? item ITEM_NONE))

(define item-not-none? (negate item-none?))

(define-catch (ns-decompose s)
  (let* ((parts (string-split s "/"))
        (namespace (butlast parts))
        (namespace (if (empty? namespace) NONE (first namespace)))
        (baseid (last parts)))
    (values namespace baseid)))

(define-catch (ns-compose ns baseid)
  (if (none? ns)
    baseid
    (str ns "/" baseid)))

(define-catch (baseid s)
  (let-values (((_ b) (ns-decompose s)))
    b))

(define-catch (namespace s)
  (let-values (((ns _) (ns-decompose s)))
    ns))

(define-catch (id item)
  (if (item-none? item)
    NONE
    (let ((id (hash-ref item "__id" NONE)))
      (if (item-not-none? (*ns*))
        (baseid id)
        id))))

(define-catch (idify s)
  (-> s (string-replace " " "_") string-downcase))

(define-catch (category? line)
  (re-matches? category-regexp line))

(define-catch (namespaced? id)
  (string-contains? id "/"))

(define-catch (correct-id-name? s)
  (re-matches? (re-format "^~a$" id-regexp) s))

(define-catch (special? s)
  (string-prefix? s "__"))

(define-catch (remove-specials item)
  (hash-filter (λ (k v) (not (special? k))) item))

(define-catch (ns+ s)
  (cond
    ((list? s) (map ns+ s))
    (else
      (if (and (not (namespaced? s))
               (correct-id-name? s))
        (ns-compose (*ns*) s)
        s))))

(define-catch (nodes-path? path tabtree)
  (true? (hash-ref tabtree (ns+ (last path)) #f)))

(define-catch (inherited? key)
  (string-prefix? (baseid key) "+"))

(define-catch (strong-key? key)
  (string-prefix? (baseid key) "*"))

(define-catch (strong-key key)
  (if (strong-key? key)
    key
    (ns+ (str "*" (baseid key)))))

(define-catch (destrong key)
  (deplus key "*"))

(define-catch (get-strongs item)
  (let* ((strongs (filter strong-key? (hash-keys item))))
    (for/fold
      ((res (hash)))
      ((strong strongs))
      (hash-union res (hash (destrong strong) (hash-ref item strong))))))

(define-catch (inheritance+ key)
  (if (inherited? key)
    key
    (ns+ (str "+" (baseid key)))))

(define-catch (deplus s (prefix "+"))
  (let*-values
      (((ns baseid) (ns-decompose s))
      ((deplused-baseid) (if (string-prefix? baseid prefix)
                            (substring baseid 1)
                            baseid)))
    (ns-compose ns deplused-baseid)))

(define (anon-item-id? id)
  (re-matches? #px"^_[1-9]?$" id))

(define-catch (rename-anon-item parent-id child-id)
  (if (and (not-none? parent-id) (not-none? child-id) (anon-item-id? child-id))
    (ns+ (format "~a~a" parent-id child-id))
    child-id))

(define-catch (get-item line)
  (let* (
        ;;; remove meta information
        (line (regexp-replace* #px"\\^[\\S]+" line ""))
        (line-id-matches? (re-matches? (re-format "^\t*(~a)" id-regexp) line))
        (line-id (if line-id-matches?
                    (cadar (get-matches (re-format "^\t*(~a)" id-regexp) line))
                    NONE))
        (line-id (rename-anon-item (*parent-id*) line-id))
        (line-id (ns+ line-id))
        (collect-matched-kv (λ (re line (f identity))
                                (for/hash
                                  ((chunk (regexp-match* re line #:match-select values)))
                                  (match-let* (((list _ key value __ ...) chunk))
                                    (values (ns+ key) (f value))))))
        (v-pattern (λ (value-regexp-str)
                      (re-format "(?<=\\s)(~a):(~a)(?=(\\s|$))" (pregexp key-regexp-str) value-regexp-str)))
        (all-parameters
          (collect-matched-kv
            (v-pattern "\\S+")
            line))
        (ref-parameters
          (collect-matched-kv
            (v-pattern id-regexp-str)
            line))
        (rdf-list-parameters
          (collect-matched-kv
            (v-pattern "[`][^`]*?[`]")
            line
            (λ (v)
              (hash
                "__rdf-list" "true"
                "__values" (-> v
                               (string-replace "`" "")
                               (string-split ",")
                               ((λ (ids) (map ns+ ids))))))))
        (string-parameters1
          (collect-matched-kv
            (v-pattern "[\"].*?[\"]")
            line
            (λ (v)
              (string-replace v "\"" "\""))))
        (string-parameters2
          (collect-matched-kv
            (v-pattern "['][^']*?[']")
            line
            (λ (v)
              (string-replace v "'" "\""))))
        (string-parameters (hash-union string-parameters1 string-parameters2))
        (integer-parameters
          (collect-matched-kv
            (v-pattern "[-+0-9][0-9]{0,8}")
            line))
        (date-parameters
          (collect-matched-kv
            (v-pattern "[0-9x][0-9x]\\.[01x][0-9x]\\.[0-9x]{3,4}")
            line))
        (url-parameters
          (collect-matched-kv
            (v-pattern "http.*?(?=[\\s])")
            line))
        (year-parameters
          (collect-matched-kv
            (v-pattern "([<>])?[1-9]{1,2}(h[12]|q[1234]|xx|[0-9]x)")
            line))
        (multiple-parameters
          (collect-matched-kv
            (v-pattern "[^\"`]\\S*,[\\S,]+")
            line
            (λ (vs)
              (string-split vs ","))))
        (multiple-ref-parameters
          (collect-matched-kv
            (v-pattern "[^\"`][A-Za-zА-ЯЁа-яёα-ωØÅøå0-9_/\\-]*,[A-Za-zА-ЯЁа-яёα-ωØÅøå0-9_,/\\-]+")
            line
            (λ (vs)
              (map ns+
                (string-split vs ",")))))
        (multiple-integer-parameters
          (collect-matched-kv
            (v-pattern "[-+0-9]{1,9},[-+0-9,]+")
            line
            (λ (vs)
              (string-split vs ","))))
        (multiple-url-parameters
          (collect-matched-kv
            (v-pattern "http[^, ]+,.+(?=[\\s])")
            line
            (λ (vs)
              (string-split vs ","))))
        (multiple-string-parameters
          (collect-matched-kv
            (v-pattern "[\"`][^\"`]*?[\"`],.+?[\"`]")
            line
            (λ (vs)
              (map
                (λ (v) (regexp-replace* #rx"[\"\\]" v ""))
                (regexp-split #px"(?<=[\"`])," vs)))))
        (parameters (hash-union
                      all-parameters
                      rdf-list-parameters
                      string-parameters
                      ref-parameters
                      url-parameters
                      year-parameters
                      date-parameters
                      integer-parameters
                      multiple-parameters
                      multiple-ref-parameters
                      multiple-integer-parameters
                      multiple-string-parameters
                      multiple-url-parameters)))
    ; add id to item, if parsed successfuly
    (cond
      ((not-none? line-id)
        (hash-union parameters (hash "__id" (ns+ line-id))))
      (else
        ITEM_NONE))))

(define-catch (merge-item-vals v1 v2)
  (let ((result
          (cond
            ((none? v1) v2)
            ((none? v2) v1)
            ((and (string? v1) (string? v2))
              (if (equal? v1 v2)
                v1
                (list v1 v2)))
            ((and (list? v1) (string? v2))
              (cons v2 v1))
            ((and (string? v1) (list? v2))
              (cons v1 v2))
            ((and (list? v1) (list? v2))
              (append v1 v2))
            ((and (hash? v1) (hash? v2))
              (hash-union v1 v2))
            (else
              (raise (format "Wrong types when merging values: ~a of type '~a' and ~a of type '~a'" v1 (type v1) v2 (type v2)))))))
      (if (list? result)
        (remove-duplicates result)
        result)))

; (: item+ : (-> Item Item Item))
(define-catch (item+ item new-parameters)
  (hash-union item new-parameters #:combine merge-item-vals))

; (: item- : (-> Item (Listof String) Item))
(define-catch (item- item keys-to-remove)
  (hash-filter
    (λ (k v) (not (index-of? keys-to-remove k)))
    item))

; (: tabtree- : (-> ItemId Tabtree Tabtree))
(define-catch (tabtree- item-id tabtree)
  (hash-delete tabtree item-id))

(define-catch (get-parameter-by-key-item key item)
  ; (when (equal? key "a") (--- item))
  (let ((inherited-key (inheritance+ key)))
    (ns+
      (or
        (hash-ref item (strong-key key) #f)
        (hash-ref item key #f)
        (hash-ref item (inheritance+ key) NONE)))))

(define-catch (get-parameter-by-key-item-id key item-id tabtree)
  (let ((item (hash-ref tabtree item-id ITEM_NONE)))
    (get-parameter-by-key-item key item)))

; (: get-parameter : (-> (Listof String) Tabtree (U String (Listof String))))
(define-catch (get-parameter path tabtree)
  (let* ((path (list->vector path))
        (parameter-key (ns+ (vector-ref* path -1)))
        (item-id (ns+ (vector-ref* path -2))))
    (get-parameter-by-key-item-id parameter-key item-id tabtree)))

; (: -> Item Item)
(define-catch (get-inherities item)
  (hash-filter
    (λ (k v) (inherited? k))
    item))

(define-catch (count-tabs line)
  (let loop ((count 0)
            (line (string-explode line)))
    (cond
      ((equal? (first line) "\t") (loop (+ 1 count) (rest line)))
      (else count))))

(define-catch (incorporate-inherities inherities)
  (hash-map
    (λ (k v) (values (deplus k) v))
    inherities))

(define-catch (fill-tree-iter source-lines (global-inherities (hash)))
  (let loop ((source-lines source-lines))
    (match-let*
        ((root-line (first source-lines))
        (line-number (~a (add1 (index-of (*source-lines*) root-line))))
        (next-lines (rest source-lines))
        (root-tabs-count (count-tabs root-line))
        (root-item (get-item root-line))
        (root-id (hash-ref root-item "__id" NONE))
        (old-root-item (hash-ref (*tabtree*) root-id ITEM_NONE))
        (local-inherities (get-inherities root-item))
        (all-inherities (hash-union global-inherities local-inherities #:combine merge-item-vals))
        (root-item (item- root-item (hash-keys local-inherities)))
        ((list sublines next-block-lines)
          (split-with
            (λ (line) (> (count-tabs line) root-tabs-count))
            next-lines))
        (top-sublines
          (filter
            (λ (line) (= (count-tabs line) (inc root-tabs-count)))
            sublines))
        (old-children-ids (hash-ref root-item "__children" empty))
        (new-children-ids
          (parameterize ((*parent-id* root-id))
            (filter
              not-none?
              (map
                (λ (%)
                  (->> % get-item id ns+))
                top-sublines))))
        (new-children-ids (if (and (not-empty? old-children-ids) (list? old-children-ids))
                            (remove-duplicates (append old-children-ids new-children-ids))
                            new-children-ids))
        (root-item (if (empty? new-children-ids)
                      root-item
                      (item+ root-item (hash "__children" new-children-ids))))
        (old-parent-ids (hash-ref root-item "__parent" NONE))
        (new-parent-ids (cond
                          ((and (not-none? old-parent-ids) (list? old-parent-ids))
                            (cons (*parent-id*) old-parent-ids))
                          ((not-none? old-parent-ids)
                            (list old-parent-ids (*parent-id*)))
                          (else (*parent-id*))))
        ; (_ (--- root-id old-parent-ids))
        (new-parent-ids (if (list? new-parent-ids)
                          (remove-duplicates new-parent-ids)
                          new-parent-ids))
        (new-parent-ids (if (and
                              (list? new-parent-ids)
                              (= 1 (length new-parent-ids)))
                          (car new-parent-ids)
                          new-parent-ids))
        (root-item (item+ root-item (hash "__parent" new-parent-ids)))
        (root-item (item+ (incorporate-inherities global-inherities) root-item))
        (root-item (hash-union root-item (get-strongs root-item)))
        (root-item (item- root-item (->> root-item hash-keys (filter strong-key?))))
        (root-item (hash-union
                      (hash-union old-root-item root-item
                        #:combine merge-item-vals)
                      (hash "__id" root-id "__line" (~a line-number))))
        (old-tabtree (*tabtree*))
        (duplicated-id? (hash-ref old-tabtree root-id #f)))
      (when duplicated-id?
          (let* (
                (old-root-line (hash-ref old-root-item "__line")))
            (if (hash-ref-path (*parse-info*) (list "duplicated-ids" root-id))
              (*parse-info* (hash-update* (*parse-info*) (list "duplicated-ids" root-id) (λ (v) (append-elements v line-number))))
              (*parse-info* (hash-set* (*parse-info*) (list "duplicated-ids" root-id) (list old-root-line line-number))))))
      (*tabtree* (hash-union
                    old-tabtree
                    (hash root-id root-item)
                    (if (empty? sublines)
                      (hash)
                      (parameterize ((*parent-id* root-id))
                        (fill-tree-iter sublines all-inherities)))
                    #:combine
                      (λ (item1 item2)
                        (hash-union item1 item2
                          #:combine merge-item-vals))))
      (cond
        ((empty? next-block-lines) (*tabtree*))
        (else (loop next-block-lines))))))

(define-catch (get-parent child-id tabtree)
  (let* ((items (hash-values tabtree))
        (result (filter
                    (λ (item)
                      (let ((children (hash-ref item "__children" NONE)))
                        (and
                          (item-not-none? children)
                          (list? children)
                          (index-of? children child-id))))
                    items))
        (result (if (not-empty? result)
                  (first result)
                  ITEM_NONE))
        (result (hash-ref result "__id" NONE)))
    result))

(define-catch (get-parent-id item)
  (hash-ref item "__parent" NONE))

(define-catch (add-hierarchy-relations item tabtree)
  (let* ((hierarchy-relation (get-parameter-by-key-item HIERARCHY_RELATION item))
        (hierarchy-inverse-relation (get-parameter-by-key-item HIERARCHY_INVERSE_RELATION item))
        (children (hash-ref item "__children" NONE))
        (parent-id (hash-ref item "__parent" NONE)))
    (hash-union
      item
      (or
        (and
           (not-none? hierarchy-relation)
           (not-none? children)
           (decart-hash hierarchy-relation children))
        (hash))
      (or
        (and
           (not-none? hierarchy-inverse-relation)
           (not-none? parent-id)
           (decart-hash hierarchy-inverse-relation parent-id))
        (hash))
      #:combine merge-item-vals)))

(define-catch (expand-file-insertions tabtree-source tabtree-filepath)
  (define global-path (get-path tabtree-filepath))
  (for/fold
    ((res empty))
    ((line tabtree-source))
    (let ((file-insertion-match (regexp-match #rx"^[^;]*\\[(.*)\\]" line)))
      (if file-insertion-match
          (let* (
                (base-tabs-number (count-tabs line))
                (filepath (second file-insertion-match))
                (local-path (get-path filepath))
                (filename (get-filename filepath))
                (full-filepath (format "~a~a~a"
                                  (if (empty-string? global-path) "" (str global-path "/"))
                                  (if (empty-string? local-path) "" (str local-path "/"))
                                  filename))
                (file-content (expand-file-insertions
                                (read-file-by-lines full-filepath)
                                full-filepath))
                (file-content (map (λ (line2) (string-append (dupstr "\t" base-tabs-number) line2)) file-content))
                )
            (append res file-content))
          (pushr res line)))))

(define-catch (parse-tabtree treefile #:namespace (namespace NONE) #:parse-info (parse-info #f) #:save-expanded-to (save-expanded-to #f))
  (define source-lines (expand-file-insertions (read-file-by-lines treefile) treefile))
  (when save-expanded-to (write-file save-expanded-to (string-join source-lines "\n")))
  (parameterize ((*ns* namespace)
                 (*parse-info* (hash "duplicated-ids" (hash)))
                 (*source-lines* source-lines))
    (let* ((lines (*source-lines*))
           (lines (clean
                    (λ (line)
                      (or
                        (re-matches? "^\t*;" line)
                        (re-matches? "^\\s*$" line)))
                    lines))
           (tabtree (parameterize ((*tabtree* (hash)))
                      (fill-tree-iter lines)))
           (tabtree (hash-map
                      (λ (k v) (values k (add-hierarchy-relations v tabtree)))
                      tabtree)))
      (if parse-info
        (*parse-info*)
        tabtree))))
