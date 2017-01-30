#lang racket
(require se3-bib/prolog/prologInScheme)
; Aufgabe 1.1
; Kann nicht unifizieren (keine Variablen)
(define (aufgabe11a)
  (unify '(pokemon Elektro Pikachu)
         '(pokemon Elektro Raichu)))
; ?typ wird an Pflanze und ?name an Bisasam gebunden
(define (aufgabe11b)
  (unify '(pokemon ?typ Bisasam)
         '(pokemon Pflanze ?name)))
; ?typ an Normal, ?name an Mauzi und ?typ an Normal
(define (aufgabe11c)
  (unify '(kampf (pokemon Normal ?name) (pokemon ?typ Ditto))
         '(kampf (pokemon ?typ Mauzi) (pokemon Normal Ditto))))
; ?name kann nicht eindeutig gebunden werden, daher #f
(define (aufgabe11d)
    (unify '(kampf (pokemon Normal ?name) (pokemon Wasser Shiggy))
           '(kampf (pokemon Normal Mauzi) (pokemon Wasser ?name))))
; Anonyme Variablen, daher #f
(define (aufgabe11e)
    (unify '(kampf (pokemon Normal ?) (pokemon Wasser Shiggy))
           '(kampf (pokemon Normal Mauzi) (pokemon Wasser ?))))
; ?name und ?name2 an Ash, ?typ and Kampf
(define (aufgabe11f)
    (unify '(team (trainer Ash) (pokemon Kampf ?name))
           '(team (trainer ?name) (pokemon ?typ ?name2))))
; ?eltern an Pikachu Ditto
(define (aufgabe11g)
    (unify '(ei Pichu . ?eltern)
           '(ei Pichu Pikachu Ditto)))
; Aufgabe 1.2
;(ausleihe Signatur Lesernummer)
(<- (ausleihe "K 110" 100))
(<- (ausleihe "P 30" 102))
(<- (ausleihe "P 32" 104))
(<- (ausleihe "P 50" 104))
;(vorbestellung Signatur Lesernummer)
(<- (vorbestellung "K 110" 104))
(<- (vorbestellung "K 110" 102))
(<- (vorbestellung "P 30" 100))
(<- (vorbestellung "P 30" 104))
; (leser Name Vorname Lesernummer Geburtsjahr)
(<- (leser Neugierig Nena 100 1989))
(<- (leser Linux Leo 102 1990))
(<- (leser Luator Eva 104 1988))

(define (aufgabe121)
  (?- (vorbestellung "P 30" ?lesernummer)))

(define (aufgabe122)
  (?- (leser Luator Eva ?geburtsjahr)))

(define (aufgabe123)
  (?- (ausleihe "P 50" ?lesernummer)(leser ?name ?vorname ?lesernummer ?)))

(define (aufgabe124)
  (?- (leser ?name ?vorname ?lesernummer ?geburtsjahr)(test (> (- 2017 60) ?geburtsjahr))))

(define (aufgabe125)
  (?- (ausleihe ?signatur1 ?lesernummer1)
      (ausleihe ?signatur2 ?lesernummer1)
      (leser ?name1 ?vorname1 ?lesernummer1 ?)
      (!= ?signatur1 ?signatur2)
      ))
