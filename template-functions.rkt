#lang racket

;; in this file we represent functions that are used in the html template files .t's

(require odysseus)
(require compatibility/defmacro)

(provide (all-defined-out))

(define (get-name item)
  (or ($ name item) ($ _name item) (namefy ($ id item))))

(define (prn param (prefix "") (postfix ""))
  (if (and param (non-empty-string? param))
    (str prefix (namefy param) postfix)
    ""))

(define (textify-interval-ru x)
  (let* ((dictionary
          (list
            (cons "-" " &ndash; ")
            (cons "<current>" "настоящее время"))))
    (for/fold
      ((res x))
      ((dict-pair dictionary))
      (string-replace res (car dict-pair) (cdr dict-pair)))))

(define (get-not-empty . args)
  (if (ormap non-empty-string? args)
    (first (filter-not empty-string? args))
    ""))

; f - how to process parameter
(define (format-if-exists frmt . fs)
  (λ args
    (let* ((parameter (apply get-not-empty args))
          (placeholder-times (length (regexp-match* "~a" frmt)))
          (fs (if (empty? fs) (dup identity placeholder-times) fs)))
      (cond
        ((znil? parameter) "")
        (else
          (apply format (apply list frmt (map (λ (f) (f parameter)) fs))))))))

(define (get-first s)
  (and (non-empty-string? s) (first (string-split s ","))))

(define take-first get-first)

(define (get-folder-name id)
  (string-replace (string-downcase id) "_" "-"))

(define-macro (calculate expr)
  `(λ (x)
      (let ((x (->number x)))
          (->string ,expr))))

(define (make-link id #:target (target "_blank") #:anchor (anchor #f) urls)
  (let* ((name (namefy id)))
    (if (and (andmap nil? urls) (not anchor))
      name
      (format "<a id=\"~a\" href=\"~a\" target=\"~a\">~a</a>"
              id
              (if anchor
                (format "#~a" (if (equal? anchor "#") id anchor))
                (httpify (for/or ((url urls)) url)))
              target
              name))))

(define make-url make-link)

; e.g. "+79113001020,+79210001112" ->
; "<ul>
;  <div>+79113001020</div>
;  <div>+79210001112</div>
;  </ul>"
(define (get-lines attrs (f identity))
  (format "<ul>~n~a~n</ul>"
    (for/fold
      ((res ""))
      ((line (string-split (->string attrs) ",")))
      (format "~a~n<li>~a</li>" res (f line)))))

(define (format-phone phone #:prefix (prefix "81554") #:landline-regex (landline-regex "^\\d{5}$"))
  (cond
    ((re-matches? "\\+\\d{11}" phone)
      (format-string "cc ccc ccc-cc-cc" phone))
    ((re-matches? "8\\d{10}" phone)
      (format-string "+7 ccc ccc-cc-cc" (ltrim phone)))
    ((re-matches? landline-regex phone)
      (format "8 (~a) ~a" prefix
                          (case (string-length phone)
                            ((4) (format-string "cc-cc" phone))
                            ((5) (format-string "c-cc-cc" phone))
                            ((6) (format-string "cc-cc-cc" phone))
                            ((7) (format-string "ccc-cc-cc" phone))
                            (else phone))))
    (else phone)))
