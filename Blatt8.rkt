#lang racket
(require se3-bib/setkarten-module)
(require se3-bib/tools-module)
(require 2htdp/image)
;Aufgabe 1:
;;1. Eine Funktion ist eine Funktion höherer Ordnung, wenn sie eine oder mehrere Funktion als Argument
;;hat oder wenn sie als Ergebnis eine Funktion zurück gibt.
;;2. a) foldr ist eine Funktion höherer Ordnung. Die Funktion wurde in der Vorlesung behandelt. Sie
;; erhält in der Eingabe eine Funktion (eine Rechenoperation), die auf die Argumente einer Liste
;; angewendet werden.
;;   b) plus-oder-minus ist keine Funktion höherer Ordnung. Da "<" nur auf Zahlen angewendet werden kann,
;; muss x eine Zahl sein. Das Ergebnis ist entweder 'Plus oder 'Minus, also eine quotierte Funktion, aber
;; keine Funktion.
;;   c) Bei masala ist davon auszugehen, dass das f in der Eingabe eine Funktion ist. Es wird f auf die
;; 2 Agumente arg1 und arg2 angewandt. Also ist masala eine Funktion höherer Ordnung.
;;   d) Bei flip ist auch davon auszugehen, dass f eine Funktion ist. Es wird f auf die Argumente x und
;; y angewandt. Es ist also flip eine Funktion höherer Ordnung.
(define (plus-oder-minus x)
  (if (< x 0)
      'Plus
      'Minus))

(define (masala f arg1)
  (lambda (arg2)
    (f arg1 arg2)))

(define (flip f)
  (lambda (x y)(f y x)))
;;3. ((masala / 1) 3) liefert das Ergebnis 1/3. Es wird / auf das zweite Argument von masala und auf das
;; Argument in der Lambda-Funktion angwandt. Durch masala werden die Argumente / 1 an das Argument 3 gebunden.
;; Dann wird / für die Argumente 1 und 3 ausgeführt.
;;4. (foldl (curry * 3) 1 '(1 2 3)) evaluiert zu 162. Zunächst wird durch (curry * 3) festgelegt, dass
;; etwas mit 3 multipliziert wird. Das foldl sorgt nun dafür, dass mit dem Wert 1 als Startwert die
;; Werte in der Liste mit 3 multipliziert werden. Das ergibt zunächst die Liste '(3 6 9). Anschließend
;; werden diese drei Argumente wie folgt mit dem Startwert multipliziert: 1*3 = 3, dann 3*6 = 18, dann 18*9 = 162.
;; (map (flip cons) '(1 2 3) '(3 2 1)) liefert als Ergebnis '((3 . 1) (2 . 2) (1 . 3)).
;; Es werden nacheinander jeweils ein Argument aus den beiden Listen gewählt. Diese werden durch flip umgedreht
;; und anschließend wird durch cons aus den beiden Argumenten ein Pair.
;; (filter list? '((a b )() 1 (()))) liefert als Ergebnis '((a b) () (())). Es werden aus der gegebenen Liste
;; alle Elemente, die selber eine Liste sind, herausgefiltert und ausgegeben. Nur die 1 ist keine Liste.
;;(map (compose (curryr / 1.8)(curry - 32))'(9941 212 32 -459.67)) liefert das Ergebnis
;; '(-5505.0 -100.0 0 273.15). map sorgt dafür, dass compose auf jedes Element in den Listen angewandt wird.
;; compose sorgt dafür, dass zuerst (curry - 32) angewandt wird.
;; Das bedeutet, dass für jedes Element e in der Liste (- 32 e) berechnet wird. Das curryr sorgt dann dafür,
;; das jedes Argument der Liste durch 1,8 geteilt wird.

;Aufgabe 2:
(define testlist1 '(1 -2 4 5 -8 9 -0))
(define testlist2 '(1 -13 13 39 169 130 131 520))
;;2.1:
(define (absolutbetrag xs)
  (map abs xs))
;;2.2:
(define (durch13teilbar list)
  (filter (lambda (x) (integer? (/ x 13))) list))
;;2.3:
(define (summeGeradeGrößer3 xs)
  (foldl + 0 (filter even? (filter (curryr > 3) xs))))
;;2.4
(define (4splitOnFunc list func)
  (let ([returnListOfLists (splitListBasedOnFunction list func)])
  (values (reverse (car returnListOfLists))(reverse (car (cdr returnListOfLists))))))

(define (splitListBasedOnFunction list func)
  (foldl
   (lambda (currEle prevResult)
     (addElementToListAndReturnAsPairBasedOnFunc func currEle (car prevResult) (car (cdr prevResult)))) '(()()) list))

(define (addElementToListAndReturnAsPairBasedOnFunc func element list1 list2)
  (if (func element)
      (list (cons element list1) list2)
      (list list1 (cons element list2))))

(define (testAll)
  (values
   (absolutbetrag testlist1)
   (durch13teilbar testlist2)
   (summeGeradeGrößer3 testlist2)))
; Da wir hier values zurückgeben sollen, wird die test Funktion extra ausgeführt.
(define (test4)
  (4splitOnFunc testlist1 even?))

; Aufgabe 3
; Teilaufgabe 1
(define number '(1 2 3))
(define pattern '(waves oval rectangle))
(define mode '(outline solid hatched))
(define color '(red green blue))
; Eine Karte stellen wir simpler weise als liste mit der Reihenfolge
; anzahl pattern, mode, color
(define (randomCard) (list (returnRndListElement number) (returnRndListElement pattern) (returnRndListElement mode) (returnRndListElement color)))

;Teilaufgabe 2
; Erzeugt alle Karten
(define (generateAllCards) (cartesian-product number pattern mode color))

; Erzeugt alle Karten und Zeigt sie
; Entweder übereinander (above) oder nebeneinander (beside)
(define (generateAndShowAllCardsBeside) (showCards (generateAllCards) beside))
(define (generateAndShowAllCardsAbove) (showCards (generateAllCards) above))

; Zeigt die Karten der Liste nebeneinander
(define (showCards list func)
    (if (empty? list)
      empty-image
      (func (showCard (car list)) (showCards (cdr list) func))))
(define (showCard cardAsList)
  (show-set-card (first cardAsList) (second cardAsList) (third cardAsList) (fourth cardAsList)))
; Gibt von einer Liste ein zufälliges Element aus
; Input: Die Liste der Elemente
; Output: Ein zufälliges Element
(define (returnRndListElement merkmalListe)
  (if (empty? merkmalListe)
      '()
      (car (shuffle merkmalListe))))

; Teilaufgabe 3
; Überprüft, ob ein die Eingabewerte ein Set sind
(define (is-a-set? listOfCards)
  (let ([card1 (car listOfCards)]
        [card2 (car (cdr listOfCards))]
        [card3 (car (cddr listOfCards))])
    (foldl (lambda (ele1 ele2 ele3 prevBool)(and (all-elements-equal-or-different? ele1 ele2 ele3) prevBool)) #t card1 card2 card3)))

(define (testIsASet?)
  (values
   (is-a-set? '((2 red oval hatched )(2 red rectangle hatched)(2 red wave hatched)))
   (is-a-set? '((2 red rectangle outline)(2 green rectangle outline )(1 green rectangle solid)))))
(define (testIsASetRandom?)
  (let* ([card1 (randomCard)]
        [card2 (randomCard)]
        [card3 (randomCard)]
        [listOfTheCards (list card1 card2 card3)])
    (values
    (showCards listOfTheCards beside)
    (is-a-set? listOfTheCards))))
    

; Wenn alle Elemente gleich sind (d.h. alle equals true) dann #t
; Wenn alle Elemente ungleich sind (d.h. alle equals false) dann #t
; Sonst #f
; and ist nur dann #t, wenn alle equals #t sind
; nor ist nur dann #t, wenn alle equals #f sind
; Daher würde statt xor auch or reichen, da nie beide Bedienungen #t sein können
(define (all-elements-equal-or-different? ele1 ele2 ele3)
  (xor (and (equal? ele1 ele2) (equal? ele2 ele3) (equal? ele1 ele3)) (nor (equal? ele1 ele2) (equal? ele2 ele3) (equal? ele1 ele3))))