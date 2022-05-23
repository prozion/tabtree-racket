#lang racket

(require rackunit)
(require odysseus)
(require "../tabtree2.rkt")
(require "../utils2.rkt")

(define countries (parse-tab-tree "fixtures/countries.tree"))
(define countries-namespaced (parse-tab-tree "fixtures/countries.tree" #:namespace 'test))
; ($t countries.europe.norway.Oslo.start countries)
; (---- countries-namespaced)

(module+ test
  ; check subtrees
  (check-equal? (length (hash-keys ($t countries.europe countries))) 9)

  ; check parameters reading
  (check-equal? ($t countries.europe.norway.Oslo.start countries) 1050)
  (check-equal? ($t countries.europe.sweden.Stockholm.regions countries) "Gamlastan")
  (check-equal? ($t countries.europe.russia.Moscow.metro-stations countries) '(Таганская Баррикадная Речной_Вокзал))

  ; check inherities
  (check-equal? ($t countries.europe.russia.Taganrog.feature countries) "сень благодати Божией")

  ; check namespaces
  (check-equal? ($t countries.europe.norway.Oslo.rdf/type countries) 'dbpedia/City)
  (check-equal? ($t countries.europe.norway.Oslo.rdf/type countries-namespaced 'test) 'dbpedia/City)
  (check-equal? ($t countries.europe.russia.Taganrog.it-companies countries-namespaced 'test) '(test--Arcadia test--Oggetto test--Dunice test--LodossTeam))
)
