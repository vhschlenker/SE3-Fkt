#lang racket
(require se3-bib/butterfly-module)
;(show-butterfly 'red 'stripes 'curved 'rhomb)

;Definition der Merkmale
;Listen beginnen mit dem dominantesten Merkmal
(define musterung '(star dots stripes))
(define fluegelfarbe '(green red blue yellow))
(define fuehlerform '(curved curly straight))
(define fluegelform '(rhomb ellipse hexagon))

; Jeder Schmetterling hat als dominates Merkmal eines von jeder Liste
; Und eine zufälliges rezessives.
; Die dominaten Merkmale kommen von den Eltern,
; Die rezesiven müssen zufällig erzeugt werden.
; Ein Kind wird nun erzeugt, in dem es zufällig entweder das dominate
; oder das rezessive Merkmal vom Elternteil kriegt.
; Sichtbar ausgeben werden wir das dominate Merkmal.
; Daher sollte der Programmablauf ungefähr so aussehen:
; 1. Es wird eine Funktion (mendeln) aufgerufen, die eine Liste der dominaten Merkmale der Eltern und der Kinderanzahl nimmt.
; 2. Zu jedem dominaten Merkmal der Eltern wird ein rezessives erzeugt (Pairs)
; 3. Diese Liste von Pairs wird einem "Kindergenerator" übergeben(der je nach Kinderanzahl oft aufgerufen wird)
; 4. Der Kindergenrator geht über jede Eigenschaft, wählt aus dem Pair ein zufälliges Merkmal
; und erzeugt damit eine Liste der von jedem Elternteil übergebenen Merkmale.
; Diese müssen dann je nach Dominanz gewählt werden, um das Kind zu erzeugen
; Daher folgende Gliederung:
; - Liste mit Merkmalen
; - Funktion "mendeln"
; Input: die Dominaten Merkmale und die Kinderanzahl
; Output: die Kinder als Bild
; - Funktion "getDominant"
; Input: Zwei Eigenschaften
; Output die Dominate Eigenschaft
; - Funktion "generateRecessive"
; Input: Eine Eigenschaft
; Output: Eine rezessive Eigenschaft dazu

; Datenstruktur
; Die Schmetterlinge werden als Liste von Pairs repräsentiert:
; Die Liste enthält die Pairs in folgender Reihenfolge: musterung, fluegelfarbe,, fuhelerform, fluegelform
; Die Pairs enthalten die Eigenschaftenn nach folgendem Muster: (Merkmal . rezessives Merkmal)

; Erzeugt bei Angabe der Dominaten Merkmale der Eltern (als Liste) n-viele Kinder
; Input: 1. Liste mit Merkmalen, 2. Liste mit Merkmalen, Kinderanzahl
; Output die Kinder
(define (mendeln merkmalListe1 merkmalListe2 kinderAnzahl)
  (cond
    [(> kinderAnzahl 0)
      (display (show (childGen (pairWithRec merkmalListe1) (pairWithRec merkmalListe2))))
      (mendeln merkmalListe1 merkmalListe2 (- kinderAnzahl 1))
     ]
      (else (void))))

; Erzeugt eine Liste mit Merkmalen eines Kindes aus den Merkmalen der Eltern
; Input: Die Liste mit Pairs der Eigenschaften vom ersten Elternteil, Die Liste mit Pairs der Eigenschaften vom zweiten Elternteil
; Output: Eine Liste mit Merkmalen, die aus den dominaten Merkmalen der beiden zufällig ausgewählten Elterneigenschaften, besteht.
(define (childGen merkmalPairListe1 merkmalPairListe2)
  (list
   (returnDominant (returnRndPairElement (car merkmalPairListe1)) (returnRndPairElement(car merkmalPairListe2)) musterung)
   (returnDominant (returnRndPairElement (cadr merkmalPairListe1)) (returnRndPairElement(cadr merkmalPairListe2)) fluegelfarbe)
   (returnDominant (returnRndPairElement (caddr merkmalPairListe1)) (returnRndPairElement(caddr merkmalPairListe2)) fuehlerform)
   (returnDominant (returnRndPairElement (cadddr merkmalPairListe1)) (returnRndPairElement(cadddr merkmalPairListe2)) fluegelform)))

; "Pairt" jedes Merkmal einer Liste mit einem rezessiven Merkmal
; Input: Liste mit Merkmalen
; Output: Liste mit Pairs aus (Merkmal . rezessives Merkmal)
(define (pairWithRec merkmalListe)
  (list
   (cons (car merkmalListe) (returnRndListElement(returnRecList (car merkmalListe) musterung)))
   (cons (cadr merkmalListe) (returnRndListElement(returnRecList (cadr merkmalListe) fluegelfarbe)))
   (cons (caddr merkmalListe) (returnRndListElement(returnRecList (caddr merkmalListe) fuehlerform)))
   (cons (cadddr merkmalListe) (returnRndListElement(returnRecList (cadddr merkmalListe) fluegelform)))
   ))

; Zeigt ein Schmetterling mit einer Liste von Merkmalen
; Input: Eine Liste von Merkmalen
; Output: zeigt den Schmetterling mit den Merkmalen
; Bemerkung: Bei der Aufzählung der Merkmale beim Aufgabenblatt und hier
; wird die Reihenfolge musterung-fluegelfarbe-fuehlerform-fluegelform verwendet.
; show-butterfly erwartet aber fluegelfarbe-musterung-fuehlerform-fluegelform
(define (show merkmale)
  (show-butterfly (cadr merkmale) (car merkmale) (caddr merkmale) (cadddr merkmale)))
; Gibt die rezessiveren Merkmale zu einem Merkmal zurück
; Input: Das Merkmal, Die Liste der Merkmale
; Output: Eine liste mit den rezessiven Merkmalen
; Bemerkung: Merkmale sind zu sich selber rezessiv
(define (returnRecList merkmal merkmalListe)
  (if (equal? merkmal (car merkmalListe))
      merkmalListe
      (returnRecList merkmal (cdr merkmalListe))))
; Gibt das dominatere von zwei Merkmalen zurück
; Input: Merkmal1, Merkmal2, Die Liste mit Merkmalen
; Output: Das dominatere Merkmale
(define (returnDominant merkmal1 merkmal2 merkmalListe)
  (cond
    [(equal? merkmal1 (car merkmalListe)) merkmal1]
    [(equal? merkmal2 (car merkmalListe)) merkmal2]
    [else (returnDominant merkmal1 merkmal2 (cdr merkmalListe))]))
; Gibt von einer Liste ein zufälliges Element aus
; Input: Die Liste der Elemente
; Output: Ein zufälliges Element
(define (returnRndListElement merkmalListe)
  (if (empty? merkmalListe)
      '()
      (car (shuffle merkmalListe))))
; Gibt von einem Pair ein zufälliges Elemnt aus
; Input: Das Pair
; Output: Ein zufälliges Element
(define (returnRndPairElement merkmalListe)
  (if (equal? (random 2) 0)
      (car merkmalListe)
      (cdr merkmalListe)))