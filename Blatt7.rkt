#lang racket
(require 2htdp/image)
;Aufgabe 1
(define testlist '(2 4 3))
(define testzahl 3)

;Teil 1: allgemein rekursiv:
(define (produkt_aR list zahl)
  (if (empty? list)
      '()
      (cons (* (car list) zahl)(produkt_aR (cdr list) zahl))))

;Teil 2: endrekursiv
(define (produkt_eR list zahl)
  (letrec ([innere (lambda(list ergebnis)
                     (if (empty? list)
                         (reverse ergebnis)
                         (innere(cdr list)(cons (* (car list) zahl) ergebnis ))))])
  (innere list '()))
  )

;Teil 3: hoehere Ordnung:
(define (produkt_hO list zahl)
  (map (lambda(number)
         (* zahl number))list))

;zum testen werden die drei Funktionen aufgerufen:
(produkt_aR testlist testzahl)
(produkt_eR testlist testzahl)
(produkt_hO testlist testzahl)

;Aufgabe 2:
;2.1 Zunaechst legen wir folgende Nummerierung fuer die Segmente fest:
;1. Mitte oben        2. oben links        3. oben rechts        4. Mitte
;5. links unten       6. rechts unten      7. Mitte unten
;Wir merken uns nun fuer welche Zahl welche der Leuchtdioden an oder aus sein muessen. Dies ist jeweils
;ein Pair aus einer Zahl und einer Liste der zugehoerigen Zustaende (an/aus) der Leuchtdioden. Es steht
;1 fuer an und 0 fuer aus.
(define segmentliste '((0 . '(#t #t #t #f #t #t #t))(1 . '(#f #f #t #f #f #t #f))(2 . '(#t #f #t #t #t #f #t))(3 . '(#t #f #t #t #f #t #t))
                       (4 . '(#f #t #t #t #f #t #f))(5 . '(#t #t #f #t #f #t #t))(6 . '(#t #t #f #t #t #t #t))(7 . '(#t #f #t #f #f #t #f))
                       (8 . '(#t #t #t #t #t #t #t))(9 . '(#t #t #t #t #f #t #t))))

;2.2
(define (getFarbe1 zahl)
  (if (equal? #t (caaddr(assoc zahl segmentliste)))"Red"
      "DimGrey"))
(define (getFarbe2 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)1))"Red"
      "DimGrey"))
(define (getFarbe3 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)2))"Red"
      "DimGrey"))
(define (getFarbe4 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)3))"Red"
      "DimGrey"))
(define (getFarbe5 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)4))"Red"
      "DimGrey"))
(define (getFarbe6 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)5))"Red"
      "DimGrey"))
(define (getFarbe7 zahl)
  (if (equal? #t (list-ref(list-ref(assoc zahl segmentliste)2)6))"Red"
      "DimGrey"))

(define (segmentdarstellung zahl)
  (underlay/xy(underlay/xy(underlay/xy(underlay/xy(underlay/xy(underlay/xy (underlay/xy(rectangle 100 200 "outline" "grey") 10 0 (rectangle 80 10 "solid" (getFarbe1 zahl)))
               0 10(rectangle 10 80 "solid"(getFarbe2 zahl)))90 10(rectangle 10 80 "solid" (getFarbe3 zahl)))10 90(rectangle 80 10 "solid" (getFarbe4 zahl)))
              0 100 (rectangle 10 80 "solid" (getFarbe5 zahl)))90 100 (rectangle 10 80 "solid" (getFarbe6 zahl)))10 180 (rectangle 80 10 "solid" (getFarbe7 zahl))))




