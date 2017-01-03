#lang racket
(require se3-bib/setkarten-module)
(require se3-bib/tools-module)
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
;;;Test:
(absolutbetrag testlist1)
;;2.2:
(define (durch13teilbar xs)
  (map (curry * 13) (filter integer? (map (curryr / 13) xs))))
;;;Test:
(durch13teilbar testlist2)
;;2.3:
(define (summeGeradeGrößer3 xs)
  (foldl + 0 (filter even? (filter (curryr > 3) xs))))
;;;Test:
(summeGeradeGrößer3 testlist2)

;Aufgabe 3:
;;3.1:
;;;Repräsentation der vier Eigenschaften eienr Karte (Form, Farbe, Anzahl, Füllung)
(define Form '(waves oval rectangle))
(define Farbe '(red green blue))
(define Anzahl '(1 2 3))
(define Füllung '(outline solid hatched))

;;;Repräsentation einer Karte, die eine Anzahl, eine Form, eine Füllung und eine Garbe hat.
(define Karte '(Anzahl Form Füllung Farbe))

;;3.2 Erzeugen einer Liste aller Karten:
(define ListeAllerSpielkarten
  '((3 rectangle hatched blue) (3 rectangle solid blue) (3 rectangle outline blue) (2 rectangle hatched blue) (2 rectangle solid blue) (2 rectangle outline blue)
    (1 rectangle hatched blue) (1 rectangle solid blue) (1 rectangle outline blue) (3 rectangle hatched green) (3 rectangle solid green) (3 rectangle outline green)
    (2 rectangle hatched green) (2 rectangle solid green) (2 rectangle outline green) (1 rectangle hatched green) (1 rectangle solid green)(1 rectangle hatched green)
    (3 rectangle hatched red) (3 rectangle solid red) (3 rectangle outline red) (2 rectangle hatched red) (2 rectangle solid red) (2 rectangle outline red)
    (1 rectangle hatched red) (1 rectangle solid red) (1 rectangle outline red) (3 oval hatched blue)(3 oval solid blue)(3 oval outline blue)
    (2 oval hatched blue) (2 oval solid blue) (2 oval solid blue) (1 oval hatched blue) (1 oval solid blue)(1 oval outline blue)
    (3 oval hatched green) (3 oval solid green) (3 oval outline green) (2 oval hatched green) (2 oval solid green)(2 oval outline green)
    (1 oval hatched green) (1 oval solid green) (1 oval outline green) (3 oval hatched red) (3 oval solid red)(3 oval outline red)
    (2 oval hatched red) (2 oval solid red) (2 oval outline red) (1 oval hatched red) (1 oval solid red)(1 oval outline red)
    (3 waves hatched blue) (3 waves solid blue) (3 waves outline blue)(2 waves hatched blue) (3 waves solid blue)(2 waves outline blue)
    (1 waves hatched blue) (1 waves solid blue) (1 waves outline blue) (3 waves hatched green) (3 waves solid green)(3 waves outline green)
    (2 waves hatched green) (2 waves solid green) (2 waves outline green) (1 waves hatched green) (1 waves solid green)(1 waves outline green)
    (3 waves hatched red) (3 waves solid red) (3 waves outline red) (2 waves hatched red) (2 waves solid red) (2 waves outline red)
    (1 waves hatched red) (1 waves solid red) (1 waves outline red)))

(define (show-set-card-list xs)
  (show-set-card
   (car xs)
   (cadr xs)
   (caddr xs)
   (cadddr xs)))
(define (grafischeDarstellung xs)
  (map (curry show-set-card-list)
                      xs))

;;3.3 Funktion für is-a-set?
;;;Wir erwaten als Eingabe eine Liste mit mindestens 3 Elementen. Zu Beginn eines Spiels wird die ListeAllerSpielkarten benutzt.
(define (3ZufaelligeKarten xs)
  (let ([r1 (random 1 83)])
    (let ([r2 (random 1 83)])
      (let ([r3 (random 1 83)])
        (if (and (not (equal? r1 r2))(not (equal? r2 r3))(not (equal? r1 r3)))
            (list (list-ref xs (- r1 1))(list-ref xs (- r2 1))(list-ref xs (- r3 1)))
            (list (list-ref xs 0)(list-ref xs 1)(list-ref xs 2)))))))

;;;Wir erwarten als Eingabe eine Liste mit 3 Elementen.
(define (is-a-set? xs)
  (if (and (or (and (equal? (car (car xs))(car (cadr xs)))(equal? (car (cadr xs))(car (caddr xs))))
               (not (or (equal? (car (car xs))(car (cadr xs)))(equal? (car (car xs)) (car (caddr xs)))(equal? (car (cadr xs))(car (caddr xs))))))
           (or (and (equal? (cadr (car xs))(cadr (cadr xs)))(equal? (cadr (caddr xs))(cadr (cadr xs))))
               (not (or (equal? (cadr (car xs))(cadr (cadr xs)))(equal? (cadr (car xs)) (cadr (caddr xs)))(equal? (cadr (cadr xs))(cadr (caddr xs))))))
           (or (and (equal? (caddr (car xs))(caddr (cadr xs)))(equal? (caddr (caddr xs))(caddr (cadr xs))))
               (not (or (equal? (caddr (car xs))(caddr (cadr xs)))(equal? (caddr (car xs)) (caddr (caddr xs)))(equal? (caddr (cadr xs))(caddr (caddr xs))))))
           (or (and (equal? (cadddr (car xs))(cadddr (cadr xs)))(equal? (cadddr (caddr xs))(cadddr (cadr xs))))
               (not (or (equal? (cadddr (car xs))(cadddr (cadr xs)))(equal? (cadddr (car xs)) (cadddr (caddr xs)))(equal? (cadddr (cadr xs))(cadddr (caddr xs)))))))
      #t
      #f))
