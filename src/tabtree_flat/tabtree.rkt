#lang racket

(require odysseus)
(require compatibility/defmacro)
(require (for-syntax odysseus racket/list))

(provide deplus NONE ITEM_NONE get-subtree) ; for testing
(provide parse-tabtree $t $tf)

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

(define NONE "")
(define ITEM_NONE (hash))

(define HIERARCHY_RELATION "hi-rel")
(define HIERARCHY_INVERSE_RELATION "hi-inv-rel")

(define *ns* (make-parameter NONE))
(define *parent-id* (make-parameter NONE))
(define *tabtree* (make-parameter (hash)))

(define id-regexp-str "[_A-Za-zА-ЯЁа-яёΑ-Ωα-ωØÅøå][\"#*A-Za-zА-ЯЁа-яёΑ-Ωα-ω0-9ØÅøå&@:._+/|<>\\?!\\-]*")
(define id-regexp (pregexp id-regexp-str))
(define key-regexp-str "[_a-zA-ZА-ЯЁа-яёα-ω0-9/|+\\-]+")
(define category-regexp "^\t*[_a-zа-яёα-ωøå.][A-Za-zА-Яа-яё0-9_+/\\-]+\\s*.*$")

(define (none? ns-or-id)
  (equal? ns-or-id NONE))

(define not-none? (inverse none?))

(define (item-none? item)
  (equal? item ITEM_NONE))

(define item-not-none? (inverse item-none?))

(define-catch (ns-decompose s)
  (let* ((parts (string-split s "/"))
        (namespace (but-last parts))
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
  (starts-with? s "__"))

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
  (starts-with? (baseid key) "+"))

(define-catch (inheritance+ key)
  (if (inherited? key)
    key
    (ns+ (str "+" (baseid key)))))

(define-catch (deplus s)
  (let*-values
      (((ns baseid) (ns-decompose s))
      ((deplused-baseid) (if (starts-with? baseid "+")
                            (substring baseid 1)
                            baseid)))
    (ns-compose ns deplused-baseid)))

(define (anon-item-id? id)
  (re-matches? #px"^_[1-9]?$" id))

(define (rename-anon-item parent-id child-id)
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
              (string-replace v "\"" ""))))
        (string-parameters2
          (collect-matched-kv
            (v-pattern "['][^']*?[']")
            line
            (λ (v)
              (string-replace v "'" ""))))
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
    (if (not-none? line-id)
        (hash-union parameters (hash "__id" (ns+ line-id)))
        ITEM_NONE)))

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
    (λ (k v) (not (indexof? keys-to-remove k)))
    item))

; (: tabtree- : (-> ItemId Tabtree Tabtree))
(define-catch (tabtree- item-id tabtree)
  (hash-delete tabtree item-id))

(define-catch (get-parameter-by-key-item key item)
  (let ((inherited-key (inheritance+ key)))
    (ns+
      (or
        (hash-ref item key NONE)
        (hash-ref item inherited-key NONE)))))

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
            (line (explode line)))
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
        (next-lines (rest source-lines))
        (root-tabs-count (count-tabs root-line))
        (root-item (get-item root-line))
        (root-id (hash-ref root-item "__id" NONE))
        (old-root-item (hash-ref (*tabtree*) root-id ITEM_NONE))
        (local-inherities (get-inherities root-item))
        (all-inherities (hash-union global-inherities local-inherities))
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
        (root-item (hash-union
                      (hash-union old-root-item root-item
                        #:combine merge-item-vals)
                      (hash "__id" root-id)))
        (old-tabtree (*tabtree*)))
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
                          (indexof? children child-id))))
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

(define-catch (get-subtree path tabtree)
  (define (collect-children next-children (checked-children empty))
    (cond
      ((empty? next-children) checked-children)
      (else
        (let* ((next-child (first next-children))
              (checked-children (pushr checked-children next-child))
              (new-children (or
                              (hash-ref
                                (hash-ref tabtree next-child (hash))
                                "__children"
                                #f)
                              empty))
              (new-next-children
                (remove-duplicates
                  (append
                    (rest next-children)
                    (minus new-children checked-children)))))
          (collect-children new-next-children checked-children)))))
  (let ((children-ids (collect-children (list (last path)))))
    (hash-filter (λ (k _)  (indexof? children-ids k)) tabtree)))

(define-catch (parse-tabtree treefile #:namespace (namespace NONE))
  (parameterize ((*ns* namespace))
    (let* ((lines (read-file-by-lines treefile))
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
      tabtree)))

(define-catch ($tf path tabtree ns)
  (parameterize ((*ns* (or ns NONE)))
    (cond
      ((nodes-path? path tabtree)
        (hash-ref
          (hash-ref tabtree (last path) ITEM_NONE)
          "__children"
          NONE))
      (else
        (get-parameter path tabtree)))))

(define-macro ($t path tabtree . ns)
  (let ((path (-> path ~a (string-split ".")))
        (namespace (if (empty? ns) #f (first ns))))
    `($tf '(,@path) ,tabtree ,namespace)))
