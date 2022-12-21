#lang racket

(require rackunit)
(require odysseus)
(require "../tabtree.rkt")

(define countries (parse-tabtree "fixtures/countries.tree"))
(define countries-namespaced (parse-tabtree "fixtures/countries.tree" #:namespace "test"))
(define foobars (parse-tabtree "fixtures/foobar.tree"))
(define foobars-namespaced (parse-tabtree "fixtures/foobar.tree" #:namespace "test"))
; ($t countries.europe.norway.Oslo.start countries)
; (---- countries-namespaced)

(module+ test
  ; check deplus
  (check-equal? (deplus "bar") "bar")
  (check-equal? (deplus "+foo") "foo")
  (check-equal? (deplus "type/rdf") "type/rdf")
  (check-equal? (deplus "type/+rdf") "type/rdf")

  ; check subtrees
  (check-equal? (length (hash-keys (get-subtree '("countries" "europe") countries))) 12)

  ; check ids reading
  (check-equal? ($t countries.africa countries) '("egypt" "tunisia"))
  (check-equal? ($t africa countries) '("egypt" "tunisia"))
  ; (check-true (indexof? ($t countries.neverland countries) "20-й_город_Ёлки"))
  (check-false (indexof? ($t countries.neverland countries) "20-й_город_Ёлки"))

  ; check parameters reading
  (check-equal? ($t countries.europe.sweden.Stockholm.regions countries) "Gamlastan")
  (check-equal? ($t countries.europe.russia.Moscow.metro-stations countries) '("Таганская" "Баррикадная" "Речной_Вокзал"))

  ; check string reading
  (check-equal? ($t foo.bar.string foobars) "Однажды, quux and foox decided to walk northwards")

  ; check dates reading
  (check-equal? ($t countries.europe.norway.Stockholm.start countries) "1252")
  (check-equal? ($t countries.europe.norway.Taganrog.start countries) "16q4")
  (check-equal? ($t countries.europe.norway.Oslo.start-lower countries) "1050")
  (check-equal? ($t personalities.Юрий_Антонов.bdate foobars) "19.02.1945")
  (check-equal? ($t personalities.Снусмумрик.bdate foobars) "05.1925")

  ; check text reading
  (check-equal? ($t countries.europe.russia.Taganrog.Греческая_47.фото countries) NONE)

  ; check multiple parameters reading
  (check-equal? (length ($t foo.bar.index foobars)) 3)
  (check-equal? ($t foo.bar.index foobars) '("1" "2" "3"))
  (check-equal? ($t foo.bar.refs foobars) '("a" "b20" "Глушко_29"))
  (check-equal? ($t foo.bar.url foobars) '("vk.com/foo" "taganrog.su"))
  (check-equal? ($t foo.bar.another-url foobars) '("http://vk.com/foo" "https://taganrog.su"))
  (check-equal? ($t foo.bar.multiple-strings foobars) '("qux" "foo and bar" "foo,bar,quux" "foo, bar and quux"))

  ; check inherities
  (check-equal? ($t countries.europe.russia.Taganrog.feature countries) "RUNNING")
  (check-equal? ($t foo.i foobars) NONE)
  (check-equal? ($t foo.bar.i foobars) "100")

  ; check namespaces
  (check-equal?
    ($t countries.europe.norway.Oslo.rdf/type countries)
    "dbpedia/City")
  (check-equal?
    ($t countries.europe.norway.Oslo.rdf/type countries-namespaced "test")
    "dbpedia/City")
  (check-equal?
    ($t countries.europe.russia.Taganrog.it-companies countries-namespaced "test")
    '("test/Arcadia" "test/Oggetto" "test/Dunice" "test/LodossTeam"))
  (check-equal?
    ($t namespaces.zendix.pars foobars)
    '("starwars/Luke_Skywalker" "startrack/Captain_Kirk" "Dar_Veter"))
  (check-equal?
    ($t namespaces.zendix.pars foobars-namespaced "test")
    '("starwars/Luke_Skywalker" "startrack/Captain_Kirk" "test/Dar_Veter"))

  ; check direct hierarchy
  (check-true (indexof? ($t quux.subfoo foobars) "scipadoo"))
  (check-equal? ($t Nucleus.consists-of foobars) '("Nuclear_membrane" "Nucleoplasm"))

  ; check inverse hierarchy
  (check-equal? ($t section1.Абрикосовая.Абрикосовая_10.street foobars) "Абрикосовая")
  (check-equal? ($t section1.Абрикосовая.Абрикосовая_10.street foobars-namespaced "test") "test/Абрикосовая")
  (check-equal? ($t Peroxisome.part-of foobars) "Cytosol")
  (check-equal? ($t Cytosol.Endosome.part-of foobars) '("Cytosol" "Compartment"))

  ; check anonymous items naming
  (check-equal? ($t foo.quux.subfoo foobars) '("scipadoo" "quux_" "quux_1"))
)
