#lang racket

(require odysseus)
(require tabtree/tabtree1)
(require "main.rkt")
(require "utils1.rkt")

(provide (all-defined-out))

(define-namespace-anchor anchor)
(define ns-own (namespace-anchor->namespace anchor))

(define <tabtree> (make-parameter (hash)))
(define <iteratees> (make-parameter (hash)))

(define (get-ht-item-name item)
  (titlefy (or ($ name item) ($ __id item) "")))

; (define (url-if-possible name url)
;   (if (and url (non-empty-string? url))
;     (format "<a href=\"~a\" target=\"_blank\">~a</a>" (httpify url) (namefy name))
;     (namefy name)))

(define (namefy-with-url id name . urls)
  (let* (
        (urls (filter-not nil? urls))
        (url (and (not-empty? urls) (car urls)))
        (title (if (non-empty-string? name) name (namefy id))))
    (if (and url (non-empty-string? url))
      (format "<a href=\"~a\" target=\"_blank\">~a</a>" (httpify url) title)
      title)))

(define (extend-txt prefix (suffix ""))
  (λ (txt)
      (if (and txt (non-empty-string? txt))
        (str prefix txt suffix)
        "")))

(define escape-txt
  (change-text '(("%(" . "% (") (")%" . ") %"))))

(define-catch (fullpath-to-data tabtree-root tabtree-path)
  (let* (
        (tabtree-path (split (->string tabtree-path) "."))
        (tabtree-file (first tabtree-path))
        (tab-tree-filepath (string->path (str tabtree-root "/" tabtree-file ".tree")))
        (tab-mtree-filepath (string->path (str tabtree-root "/" tabtree-file ".mtree")))
        (mtree? (file-exists? tab-mtree-filepath))
        (tree? (file-exists? tab-tree-filepath))
        (_ (when (and (not mtree?) (not tree?)) (error (format "process-html-template: no corresponded tabtree file for the path: ~a" tabtree-path))))
        (tabtree (if mtree? (parse-tab-mtree tab-mtree-filepath) (parse-tab-tree tab-tree-filepath)))
        (datapath (cdr tabtree-path))
        (data (if (empty? datapath) tabtree (or (get-$1 datapath tabtree) (get-$2 datapath tabtree))))
        )
    data))

(define-catch (fill-template data html-template #:ns (ns ns-own))
  (cond
    ((hash-empty? data) "")
    (else
      (let* (
            ;; replace keyword <this> with a hash object (data)
            (html-template (string-replace html-template "<this>" (~s data)))
            (snippets (map
                        (λ (x) (second x))
                        (get-matches #rx"{(.*?)}" html-template)))
            (result (for/fold
                      ((res html-template))
                      ((snippet snippets))
                      (let* (
                            (snippet-lst (split snippet ":"))
                            (snippet-varnames (split (first snippet-lst) ","))
                            (snippet-lambda (if (> (length snippet-lst) 1) (cdr snippet-lst) #f))
                            (snippet-lambda (cond
                                              ((not snippet-lambda) snippet-lambda)
                                              ((equal? "" snippet-lambda) #f)
                                              ; if some ':'s in lambda itself - merge on : again
                                              ((> (length snippet-lambda) 1) (implode snippet-lambda ":"))
                                              (else (car snippet-lambda))))
                            (snippet-lambda (if snippet-lambda
                                              ; (eval (read (open-input-string (str "(λ args (apply " snippet-lambda " args))"))) ns-own)
                                              (eval (read (open-input-string snippet-lambda)) ns)
                                              (λ args (implode args ",")))) ; leave e.g. "a,b,c" as it was
                            (prefix-string (if (> (length snippet-lst) 2) (third snippet-lst) ""))
                            (postfix-string (if (> (length snippet-lst) 3) (fourth snippet-lst) ""))
                            (result-core (apply
                                            snippet-lambda
                                            (map
                                              (λ (x) (cond
                                                      ((re-matches? "\"\"" x) #f)
                                                      ((re-matches? "\".*?\"" x) (triml (trimr x)))
                                                      ((re-matches? "[0-9]+" x) (->number x))
                                                      (else (hash-ref* data x #f))))
                                              snippet-varnames)))
                            (result (if (and result-core (non-empty-string? result-core))
                                      result-core
                                      ""))
                            )
                        (string-replace
                          res
                          (str "{" snippet "}")
                          result
                          )))))
        result))))

(define-catch (snippet->data snippet #:ns namespace #:tabtree-root (tabtree-root "."))
  (let* (
        (snippet-parts (string-split snippet ":"))
        (collection-part (first snippet-parts))
        (collections (string-split collection-part ","))
        (collection-paths (map (λ (x) (let* ((collection-part-parts (string-split x "//"))
                                            (path-part (if (> (length collection-part-parts) 1) (second collection-part-parts) (first collection-part-parts))))
                                          path-part))
                                collections))
        (tabtrees (for/fold
                    ((res (list)))
                    ((collection collections))
                    (let* ((collection-lst (string-split collection "//"))
                          (tabtree-root (if (= (length collection-lst) 1) tabtree-root (first collection-lst)))
                          (tabtree-path (if (= (length collection-lst) 1) (first collection-lst) (second collection-lst)))
                          (tabtree-path (split tabtree-path "."))
                          (tabtree (fullpath-to-data tabtree-root (first tabtree-path))))
                      (pushr res tabtree))))
        (function-part (cond
                          ((> (length snippet-parts) 2)
                              (eval (read (open-input-string (implode (cdr snippet-parts) ":"))) namespace))
                          ((= (length snippet-parts) 2)
                              (eval (read (open-input-string (second snippet-parts))) namespace))
                          (else identity)))
        (parameter-values (map (λ (collection-path tabtree) (get-$1 (rest (split collection-path ".")) tabtree)) collection-paths tabtrees))
        (result (apply function-part parameter-values)))
    result))

(define-catch (process-html-template filepath #:tabtree-root (tabtree-root "") #:namespace (namespace #f))
  (define-catch (get-cycle-object lst)
    (let* (
          (template-html (first lst))
          (collection-part (split (second lst) ":"))
          (collection (first collection-part))
          (collection-lst (string-split collection "//"))
          (tabtree-root (if (= (length collection-lst) 1) tabtree-root (first collection-lst)))
          (tabtree-path (if (= (length collection-lst) 1) (first collection-lst) (second collection-lst)))
          (tabtree-path (split tabtree-path "."))
          (html-part (third lst))
          (tabtree (fullpath-to-data tabtree-root (first tabtree-path)))
          (_ (<tabtree> tabtree))
          (_ (<iteratees> (get-leaves tabtree)))
          (snippet-lambda (if (> (length collection-part) 1) (cdr collection-part) #f))
          (snippet-lambda (cond
                            ((not snippet-lambda) snippet-lambda)
                            ((equal? "" snippet-lambda) #f)
                            ((> (length snippet-lambda) 1) (implode snippet-lambda ":"))
                            (else (car snippet-lambda))))
          (snippet-lambda (if snippet-lambda
                            (eval (read (open-input-string snippet-lambda)) namespace)
                            identity))
          (cycled-data (get-$3 (cdr tabtree-path) tabtree))
          ; do filtering, mapping or other operations over collection of data
          (cycled-data (snippet-lambda cycled-data))
          (result (hash 'matched-template template-html 'collection collection 'data cycled-data 'html html-part))
          )
      result))
  (define-catch (execute-code-snippet code-snippet)
    (eval (read (open-input-string (str "(" code-snippet ")"))) namespace))
  (let* ((filepath (if (path? filepath) filepath (string->path filepath)))
        (html-res (read-file filepath))
        (code-snippets-before (get-matches #rx"%%\\((.+?)\\)%%" html-res))
        (code-snippets-before (map second code-snippets-before))
        (code-snippets-before-results (map execute-code-snippet code-snippets-before))
        (html-res (for/fold
                    ((res html-res))
                    ((code-snippet-r code-snippets-before-results) (code-snippet code-snippets-before))
                    (string-replace res (str "%%(" code-snippet ")%%") (if (and code-snippet-r (not (void? code-snippet-r))) (->string code-snippet-r) ""))))
        (cycle-snippets (get-matches #rx"\\[{(.+?)}:(.+?)\\]" html-res))
        (cycle-objects (map get-cycle-object cycle-snippets))
        (html-res (for/fold
                    ((res html-res))
                    ((cycle-object cycle-objects))
                    (string-replace
                      res
                      ($ matched-template cycle-object)
                      (for/fold
                        ((res2 ""))
                        ((data-item (or ($ data cycle-object) (list (hash)))))
                        (str
                          res2
                          (fill-template data-item ($ html cycle-object) #:ns namespace))))))
        (snippets (map
                    (λ (x) (second x))
                    (get-matches #rx"{{(.*?)}}" html-res)))
        (html-res (for/fold
                    ((res html-res))
                    ((snippet snippets))
                    (string-replace res (str "{{" snippet "}}") (snippet->data snippet #:ns namespace #:tabtree-root tabtree-root))))
        (code-snippets (get-matches #rx"%\\((.+?)\\)%" html-res))
        (code-snippets (map second code-snippets))
        (code-snippets-results (map execute-code-snippet code-snippets))
        (html-res (for/fold
                    ((res html-res))
                    ((code-snippet-r code-snippets-results) (code-snippet code-snippets))
                    (string-replace
                      res
                      (str "%(" code-snippet ")%")
                      (if (and code-snippet-r (not (void? code-snippet-r)))
                        (->string code-snippet-r)
                        ""))))
        ; clean empty lines after snippets
        (html-res (string-replace html-res #px"\n\\s*?\r?\n" "\n\n"))
        (html-res (string-replace html-res #px"\n\\s+?\n" "\n\n"))
        (html-res (string-replace html-res #px"(?m:\n\n+)" "\n\n"))
        )
    ; (--- html-res)
    html-res))
