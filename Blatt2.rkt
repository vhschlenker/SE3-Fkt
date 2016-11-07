#lang racket
;Aufgabe1
(define miau 'Plueschi)
(define katze miau)
(define tiger 'miau)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Sam)
        (PersonC PersonA))
        PersonC))

(define xs1 '(0 2 3 miau katze))
(define xs2 (list miau katze))
(define xs3 (cons katze miau))

;1. Ergebnis: 'Plueschi - Aus miau wird 'Plueschi, weil aufgrund der Zeile (define miau 'Plueschi) miau zu 'Plueschi ausgewertet wird.
;2. Ergebnis: 'Plueschi - Aus katze wird 'Plueschi, weil katze zu miau ausgewertet wird und anschließend miau zu 'Plueschi ausgewertet wird.
;3. Ergebnis: 'miau - tiger wird zu 'miau ausgewertet. Da vor miau noch ein ' steht, wird 'miau nicht weiter ausgewertet.
;4. Ergebnis: 'katze - Bei (quote katze) wird vor "katze" noch ein ' gesetzt, sodass das Zeichen 'katze daraus wird,.  
;5. Ergebnis: 'Plueschi - (eval tiger) wertet tiger so weit aus, wie möglich. In diesem Fall wird aus tiger zunächst 'miau. Dann wird
     ;aus 'miau ein miau und aus dem miau wiederum ein 'Plueschi. 'Plueschi kann nicht weiter ausgewertet werden.
;6. Ergebnis: Fehlermeldung: Plueschi ist undefiniert. Es wird katze zu miau ausgewertet und dann miau zu 'Plueschi. Dann wird versucht,
     ;'Plueschi zu Plueschi auszuwerten, was aber nicht funktioniert, weil Plueschi undefiniert ist,
;7. Ergebnis: miau - 'tiger wird zu tiger ausgewertet und dann zu 'miau.
;8. Ergebnis: 'harry - Es wird parallel 'harry zu 'Sam und 'PersonC zu 'harry ausgewertet. Anschließend wird PersonC (also 'harry) ausgegeben.
;9. Ergebnis: '(miau katze) - Es wird das Zeichen '(0 2 3 miau katze) von links gelesen und für jedes "d" ein Eintrag auf der linken Seite gelöscht.
      ; Es bleibt das Zeichen '(miau katze) übrig.
;10. Ergebnis: '(Plueschi) - Aus der Liste wird der linke Eintrag "miau" gelöscht. Anschließend wird katze zu miau und dann zu 'Plueschi ausgewertet. Es wird also
      ; das Zeichen '(Plueschi) ausgegeben.
;11. Ergebnis: 'Plueschi - Durch cons wird aus katze und miau ein pair "katze"."miau". Es wird durch cdr der erste Eintrag ("katze") gelöscht,
      ; sodass nur noch "miau" übrig bleibt. Dieses "miau" wird dann zu 'Plueschi ausgewertet.
;12. Ergebnis: 1.7320508075688772 - Die Wurzel von 3 wurde ausgerechnet. Danach wird eval ausgeführt, was aber das Ergebnis nicht weiter verändert, weil
      ;1.7320508075688772 nicht weiter ausgewertet werden kann.
;13. Ergebnis: 'tiger - Es wird zunächst '(welcherNameGiltWo 'tiger 'katze)) zu  (welcherNameGiltWo 'tiger 'katze)) ausgewertet. Dann werden parallel
      ; 'tiger zu 'Sam und PersonC zu 'tiger ausgewertet. Dann wird PersonC (also 'tiger) zurückgegeben. 
;14. Ergebnis: 'Plueschi - Es werden parallel 'katze zu 'Sam und PersonC zu 'katze ausgewertet. Dann wird PersonC (also 'katze so weit wie möglich
      ; ausgewertet. Dabei wird zuerst aus 'katze katze, aus katze miau und aus miau 'Plueschi. Das Ergebnis ist also 'Plueschi.

;Aufgabe2
;2.1
(define (fakultaet n)
  (if (= n 0)1(* n(fakultaet (- n 1)))))

;2.2
(define (power r n)
  (if (= r 0) 1 (if (even? n) (expt (expt r (/ n 2)) 2) (* (power r (- n 1)) r))))

; 2.3 Die Eulerzahl e
; 2,718281828459045
(define (euler)
  (exact->inexact (/ (+ 1 (sum 1)) 2)))
(define (sum n)
  (let [(result (/ (+ n 1) (fakultaet n)))]
  (if (< result (/ 1 (expt 10 1000))) result (+ result (sum (+ n 1))))))

; 2.4 π
; 3,141592653589793
(define (pi)
  (let [(precision 500)]
    (values
     (* 4 (exact->inexact (piViertel precision)))
     (exact->inexact (calcPi precision))
     )))

;2.4 Begründugn, warum Pi/4 nicht so schnell approximiert wird: Durch die Fakultät im Nenner
    ;werden kleinere und somit auch genauere Werte berechnet. Die Approximation gelangt also
    ;schneller zu den kleinen Zahlenwerten.
(define (piViertel n)
  (if (= n 1) 1 (+(piViertel (- n 1))(*(/ 1 (- (* 2 n) 1))(expt -1 (- n 1))))))

;;; ---
;; 2.4 - My attempt with slightly different results
;Only odd values for precision. Higher value leads to higher precision
(define(calcPi precision)
  (if(odd? precision)
     (* 4 (recPi precision))
     (* 4 (recPi(- precision 1)))))

(define (recPi x)
  (if(= x 1) 1
     (if(= (modulo x 4) 1)
        (+ (recPi(- x 2)) (/ 1 x))
        (- (recPi(- x 2)) (/ 1 x)))
     ))
;;; ---

;Aufgabe3
(define (type-of a)
     (cond
       [(boolean? a) 'Boolean]
       [(list? a) 'List]
       [(pair? a) 'Pair]
       [(symbol? a) 'Symbol]
       [(number? a) 'Number]
       [(char? a) 'Char]
       [(string? a) 'String]
       [(vector? a) 'Vektor]
       [(procedure? a) 'Procedure]
       [ else "No correct input"]))

(define (id z) z)
(define (test-type-of)
  (values
   (type-of (* 2 3 4)) ;'Number - Es wird 2*3*4=24 berechnet. 24 ist eine Zahl.
   (type-of (not 42)) ;'Boolean - not 42 gibt den Wert #f (false) zurück. Der Typ davon ist Boolean.
   (type-of '(eins zwei drei)) ;'List - (eins zwei drei) ist eine Liste.
   (type-of '()) ;'List - '() ist die leere Liste. Die leere Liste ist vom Typ Liste.
   (type-of (id sin)) ;'Procedure - id sin ergibt sin. sin ist vom Typ Prozedur.
   (type-of (string-ref "Harry Potter und der Stein der Weisen" 3)) ;'Char - Das Zeichen an der Stelle 4
      ;wird zurückgegeben. In diesem Fall ist das ein "r". Da r ein Zeichen (Char) ist, ist string-ref 3 vom Typ Char.
   (type-of (lambda (x) x)) ;Die Funktion lambda bildet x x nach x ab und ist vom Typ "Procedure".
   (type-of type-of) ;'Procedure - type-of ist eine Funktion, also wird 'Procedure zurück gegeben.
   (type-of (type-of type-of)))) ;'Symbol - Zunächst wird wie oben (type-of type-of) zu 'Procedure ausgewertet. 'Procedure  ist aber wiederum ein Symbol.
      ;Deswegen wird dann (type-of 'Procedure) zu 'Symbol ausgewertet.
