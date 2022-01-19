#lang racket

;; in this file we represent functions that modify elements of the tree

(require odysseus)
(require "main.rkt")
(require compatibility/defmacro)

(provide (all-defined-out))

(define (make-index items (index-name "i"))
  (for/fold
    ((res empty))
    ((item items) (idx (in-naturals 1)))
    (pushr
      res
      (hash-union
        (hash index-name idx)
        item))))

; {w,w<r,w>r:first-existed}
(define (first-existed (fallback-value #f))
  (λ args
    (let ((existed (filter-not nil? args)))
      (if (empty? existed)
        fallback-value
        (first existed)))))

(define (add-type val list-of-hashes #:attr (attr 'type))
  (map
    (λ (x) (hash-union (hash attr val) x))
    list-of-hashes))

(define-catch (tabtree-item->string item keys-order #:remove-derived-keys (remove-derived-keys #t) #:quoted-keys (quoted-keys empty))
  (define (parameter->string key params (delimeter ","))
    (cond
      ((list? params) (implode params delimeter))
      ((re-matches? "-f$" (->string key)) (format "`~a`" params)) ; a code snippet
      ((and (string? params) (re-matches? " " params)) (format "\"~a\"" params))
      ((indexof*? quoted-keys key) (format "\"~a\"" params))
      (else (->string params))))
  (let* ((id ($ __id item))
        (keys-to-print (filter-not (λ (x) (let ((x (->string x))) (or (string-prefix? x "_") (equal? x "id")))) (hash-keys item)))
        (keys-to-print (if remove-derived-keys
                            (filter-not (λ (k) (regexp-match? #rx"^\\+.+" (->string k))) keys-to-print)
                            keys-to-print))
        (keys-ordered (if keys-order
                            (for/fold
                              ((res empty))
                              ((ko keys-order))
                              (if (indexof*? keys-to-print ko)
                                (pushr res (->symbol ko))
                                res))
                            empty))
        (tail-keys (minus keys-to-print keys-ordered)))
    (for/fold
      ((res (format "~a" id)))
      ((key (append (or keys-ordered empty) (or tail-keys empty))))
      (format "~a ~a:~a" res key (parameter->string key (hash-ref item key))))))

(define-catch (tabtree->string
                  tabtree
                  #:parent (parent-item #f)
                  #:level (level 0)
                  #:keys-order (keys-order #f)
                  #:quoted-keys (quoted-keys empty)
                  #:sort-f (sort-f (λ (a b) (< (->number (or ($ _order a) 0)) (->number (or ($ _order b) 0))))))
  (let* ((sorted-keys (sort (hash-keys tabtree) sort-f))
        (keys-order (or (and parent-item ($ keys-order parent-item)) keys-order)) ; such a way we can define keys-order only in the topmost element and it will be inherited by all lower elements unless they define their own keys-order
        (keys-order (cond
                      ((string? keys-order) (string-split keys-order ","))
                      ((list? keys-order) keys-order)
                      (else keys-order))))
    (for/fold
      ((res ""))
      ((k sorted-keys))
      (let* ((leaf? (hash-empty? (hash-ref tabtree k))))
        (cond
          (leaf?
            (format "~a~a~a"
                    (if (or (not-empty-string? res) parent-item) (format "~a~n" res) res)
                    (dupstr "\t" level)
                    (tabtree-item->string
                      #:quoted-keys quoted-keys
                      k
                      (or ($ keys-order k) keys-order))))
          (else
            (format "~a~a~a~a"
                    (if (or (not-empty-string? res) parent-item) (format "~a~n" res) res) ; don't skip the first line in the file
                    (dupstr "\t" level)
                    (tabtree-item->string
                      #:quoted-keys quoted-keys
                      k
                      (or ($ keys-order k) keys-order)) ; first check if current item has keys-order attribute - then apply this one, otherwise use normal keys-order from the parent
                    (tabtree->string
                      (hash-ref tabtree k)
                      #:parent k
                      #:level (+ 1 level)
                      #:keys-order keys-order
                      #:quoted-keys quoted-keys
                      #:sort-f sort-f))))))))

; hashtree -> list-of hash
(define (get-first-level hashtree)
  (hash-keys hashtree))

; hashtree -> hashtree
(define (get-level-below hashtree)
  (let* ((root-element (get-root-item hashtree)))
    (hash-ref hashtree root-element #f)))

(define default-sorter tabtree<)

(define default-sort-f tabtree<)
(define default-sort-by '__id)

(define (hash-id h)
  ($ __id h))

; Important! Tabtree file should contain only one root element. Other first level elements and their children will be deleted after sorting.
(define-catch (tabtree-sort-and-print
                  #:tabtree (tabtree #f)
                  #:tabtree-file (tabtree-file #f)
                  #:ns (ns #f)
                  #:new-treefile (new-treefile #f)
                  #:old-treefile (old-treefile #f)
                  #:keys-order (keys-order #f)
                  #:sort-by (sort-by default-sort-by)
                  #:sort-by-numeric-desc (sort-by-numeric-desc #f)
                  #:sort-by-numeric-asc (sort-by-numeric-asc #f)
                  #:sort-by-f (sort-by-f #f)
                  #:sort-f (sort-f default-sort-f))
  (define-catch (tabtree-sort-rec root-item root-hashtree #:sort-by sort-by #:sort-f sort-f #:sort-by-f sort-by-f)
    (cond
      ((not (hash? root-hashtree)) root-hashtree)
      ((hash-empty? root-hashtree) root-hashtree) ; empty list
      ((not-hashes? (hash-keys root-hashtree)) root-hashtree) ; end element
      (else
        (let* (
              (embedded-sort-by (and ($ sort-by root-item) ns (read (open-input-string ($ sort-by root-item) ns))))
              (embedded-sort-by (and embedded-sort-by (->symbol embedded-sort-by)))
              (embedded-sort-f (and ($ sort-f root-item) ns (read (open-input-string ($ sort-f root-item) ns))))
              (embedded-sort-f (and embedded-sort-f (->symbol embedded-sort-f)))
              (sort-by (or embedded-sort-by sort-by))
              (sort-f (or embedded-sort-f sort-f))
              (root-hashtree-keys (hash-keys root-hashtree))
              (sorter (λ (a b) (let ((a-val (if sort-by-f
                                                (sort-by-f a)
                                                (hash-ref a sort-by #f)))
                                    (b-val (if sort-by-f
                                              (sort-by-f b)
                                              (hash-ref b sort-by #f))))
                                  (cond
                                    ((or sort-by-numeric-desc sort-by-numeric-asc)
                                      (let* ((a-val (or (hash-ref* a sort-by-numeric-desc #f) (hash-ref* a sort-by-numeric-asc #f)))
                                            (b-val (or (hash-ref* b sort-by-numeric-desc #f) (hash-ref* b sort-by-numeric-asc #f)))
                                            (a-val (and a-val (->number a-val)))
                                            (b-val (and b-val (->number b-val)))
                                            (op (if sort-by-numeric-desc > <)))
                                        (and a-val b-val
                                            (op a-val b-val))))
                                    ((and a-val b-val)
                                      ((eval sort-f ns) a-val b-val))
                                    (else
                                      (let ((a-val (hash-ref a default-sort-by #f))
                                            (b-val (hash-ref b default-sort-by #f)))
                                      (default-sort-f a-val b-val))))))))
            (for/hash
              ((k (sort root-hashtree-keys sorter)) (i (range 1 (inc (length root-hashtree-keys)))))
              (values (hash-union (hash '__order i) k) (tabtree-sort-rec k (hash-ref root-hashtree k) #:sort-by sort-by #:sort-by-f sort-by-f #:sort-f sort-f)))))))
  (let* ((hashtree
                (cond
                  (tabtree tabtree)
                  (tabtree-file (parse-tab-tree tabtree-file))
                  (else (errorf "neither #tabtree nor #tabtree-file are given in tabtree-sort-and-print"))))
        (root-item (get-root-item hashtree))
        (hashtree-sorted (hash
                            root-item
                            (tabtree-sort-rec root-item (get-level-below hashtree) #:sort-by sort-by #:sort-by-f sort-by-f #:sort-f sort-f)))
        (new-treefile-name (if new-treefile
                              new-treefile
                              (cond
                                (tabtree-file
                                  (let*
                                      ((filename-parts (string-split tabtree-file #rx"(?<=[A-Za-zА-Яа-я0-9_])\\.(?=[A-Za-zА-Яа-я0-9_])"))
                                      (aname (car filename-parts))
                                      (ext (and (not-empty? (cdr filename-parts)) (cadr filename-parts))))
                                    (str aname "_" (if ext (str "." ext) ""))))
                                (else "_new_tabtree.tree"))))
        (new-treefile-str (tabtree->string hashtree-sorted #:keys-order keys-order)))
    (cond
      (new-treefile (write-file new-treefile-name new-treefile-str))
      (old-treefile
        (write-file old-treefile (read-file tabtree-file))
        (write-file tabtree-file new-treefile-str))
      (else (errorf "No files specified to write the result")))
    #t))

(define-catch (filter-tabtree f tabtree)
  (cond
    ((hash-empty? tabtree) tabtree)
    (else
      (let* ((filtered-keys (filter f (hash-keys tabtree))))
        (for/fold
          ((res (hash)))
          ((k filtered-keys))
          (hash-union res (hash k (filter-tabtree f (hash-ref tabtree k)))))))))

(define (set-current-page-mark id curpage-id (class "current_page_nav"))
  (if (equal? (->string id) (->string curpage-id))
    (format "class=\"~a\"" class)
    ""))

(define (tabtree-true? v)
  (cond
    ((equal? (->string v) "<t>") #t)
    ((equal? (->string v) "<f>") #f)
    (else #t)))

; get either '<key>' or inherited '<key>+' value of item
(define-macro ($* k item)
  (let ((kplus (string-append "+" (format "~a" k))))
    `(or
        (hash-ref* ,item ',k)
        (hash-ref* ,item ,kplus))))

(define-catch (without-special-keys item)
  (let ((special-keys-re "^[+_].+|id|.+?-id$"))
    (for/fold
      ((res (hash)))
      ((k (hash-keys item)))
      (cond
        ((re-matches? special-keys-re (->string k)) res)
        (else (hash-insert res (cons k (hash-ref item k))))))))

(define-catch (get-item-parameter item-id parameter-name items)
  (let* ((item (and items (@id item-id items))))
    (and item (hash-ref* item parameter-name #f))))

(define hash->tabtree
  (--> hashtree->string hash->hashtree))

(define (get-values value (sep ","))
  (cond
    ((not value) empty)
    (else (string-split value sep))))

(define-catch (extract-tabtree-frame input-file output-file)
  (let* (
        ;; take all lines from the file:
        (tree-lines (read-file-by-lines input-file))
        ;; remove comment lines:
        (tree-lines (clean
                      (λ (line) (or
                                  (re-matches? "^\t*;" line)
                                  (re-matches? "^\\s*;" line)
                                  (re-matches? "^\\s*$" line)))
                      tree-lines))
        ;; leave just categories:
        (tree-lines (filter category? tree-lines))
        )
    (write-file-by-lines output-file tree-lines))
    #t)
