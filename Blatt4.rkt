#lang racket
(require se3-bib/tools-module)

;Aufgabe 1:
;1. (max (min 5 (- 6 7)) 8) = 8 Erklärung: Es wird zunaechst 6 - 7 berechnet, was -1 ergibt. Dann wird das Minimum aus 5 und
 ;-1 gebildet. Das ist -1. Dann wird das Maximum von -1 und 8 bestimmt. Das ist 8.

;2. '(+ (- 11 13) 17) = '(+ (- 11 13) 17) Erklärung: Es wird nichts berechnet, weil ein " ' " vor dem Ausdruck steht. Es hamdelt
 ;sich hierbei um eine Liste mit einem Element.

;3. (cadr '(Alle Jahre wieder)) = 'Jahre Erklärung: Aus der Liste '(Alle Jahre wieder) 
; wird durch cadr zuerst der Rest der Liste '(Jahre wieder) und davon das
; erste Element gewählt '(Jahre).

;4. (cddr '(kommt (das Christuskind))) = '() Erlärung: Es handelt sich um eine Liste mit 2 Elementen. Das erste Element
; ist "kommt" und das zweite Element ist eine List mit den Elementen "das" und "Christuskind".
; Von dem zweiten Element '((das Christuskind)) wird der Rest der Liste ausgegeben. 
; Da die Liste nur ein Elemnt hat, wird '() zurück gegeben

;5. (cons 'Auf '(die Erde nieder)) = '(Auf die Erde nieder) Erklärung: Das Zeichen " 'Auf " und die Liste mit den
 ;3 Elementen "die", "Erde" und "nieder" werden konkateniert. Es ergibt sich eine neue Liste mit den 4 Elementen "Auf",
 ;"die", "Erde" und "nieder".

;6. (cons 'Wo 'wir) = '(Wo . wir) Erklärung: Die beiden Zeichen "Wo" und "wir" werden konkateniert. Das ergibt einen Pair mit
 ;den zwei Elementen "Wo" und "wir".


;7. (equal? (list 'Menschen 'sind) '(Menschen sind)) = #t (true) Erklärung: Es werden 2 Elemente miteinander verglichen. Das erste
 ;Element ist eine Liste, die aus den Zeichen "Menschen" und "sind" besteht. Das zweite Element ist die Liste mit den 2 Elementen
 ;"Menschen" und "sind". in beiden Fällen erhalten wir eine Liste mit den 2 Elementen "Menschen" und "sind". Die Listen sind also
 ;gleich im Sinne von equal?, werden also zu demselben Wert ausgewrtet.

;8. (eq? (list 'Rudolph 'Das 'Rentier) (cons 'Rudolph '(Das Rentier))) = #f (False) Erklärung: Es wird eine Liste mit den 3 Zeichen
 ;"Rudolph", "Das" und "Rentier" erzeugt und mit der Konkatenation von dem Zeichen "Rudolph" mit der Liste mit den 2 Elementen "Das" und "Rentier"
 ;verglichen. In beiden Fällen entsteht eine Liste mit den drei Elementen "Rudolph", "Das" und "Rentier" verglichen. Sie referenzieren
 ;aber auf unterschiedliche Werte, also wird eq? zu false ausgewertet.

;Aufgabe 2:
;2.1: <Notruf> ::= <Überschrift> <Standortangabe> <Art des Notfalls> <Notfallzeit> 'UTC' <Erforderte Hilfe> 'ICH SENDE DEN TRÄGER' <Peilzeichen> <Unterschrift> 'Over'
     ;<ÜBERSCHRIFT> ::=  <Notzeichen> <Notzeichen> <Notzeichen> <Hier ist> <Schiffsname> <Schiffsname> <Schiffsname> <Rufzeichen> <Notzeichen> <Schiffsname> 'Ich buchstabiere' <Buchstabieren> <Buchstabieren>
        ;<Notzeichen> ::= MAYDAY
        ;<Hier ist> ::= Hier ist | DELTA ECHO
        ;<Schiffsname> ::= SEASIDE | Amira
        ;<Rufzeichen> ::= ECHO | AMRY
        ;<Buchstabieren> ::= <Buchstabieren><Buchstabiert> | <Buchstabiert>
        ;<Buchstabiert> ::= ALFA | BRAVO | CHARLIE | DELTA  | ECHO | FOXTROTT | GOLF | HOTEL | INDIA | JULIETT | KILO | LIMA | MIKE
          ;MIKE | NOVEMBER | OSCAR | PAPA | QUEBEC | ROMEO | SIERRA | TANGO  | UNIFORM | VIKTOR | WHISKEY | X-RAY | YANKEE | ZULU
     ;<Standortangabe> ::= NOTFALLPOSITION UNGEFÄHR 10 SM NORDÖSTLICH LEUCHTTURM KIEL | 57°46’N, 006°31’E
     ;<Notfallzeit> ::= 0 | 1 | 2 | ... | 100000 | ...
     ;<Art des Notfalls> ::= SCHWERER WASSEREINBRUCH WIR SINKEN | Kenterung in schwerer See
     ;<Erforderte Hilfe> ::= <Erforderte Hilfe> <Zustand> | <Zustand>
       ;<Zustand> ::= KEINE VERLETZTEN | VIER MANN GEHEN IN DIE RETTUNGSINSEL | SCHNELLE HILFE ERFORDERLICH
     ;<Peilzeichen> ::= -- 
     ;<Unterschrift> ::= SEASIDE SIERRA SIERRA DELTA ECHO

;2.2
(define (notmeldungGenerieren schiffsname rufzeichen position artDesNotfalls erforderteHilfe)
  (string-append (ueberschrift schiffsname rufzeichen)"\n" position "\n" artDesNotfalls "\n"
                 erforderteHilfe "\n" (peilzeichen) "\n" (unterschrift schiffsname rufzeichen) "\n" "OVER \n \n" ))
    (define (ueberschrift schiffsname rufzeichen ) (string-append (notzeichen) "\n" (hierIst) "\n" schiffsname " "
                                                                  schiffsname " " schiffsname " "(buchstabierenR rufzeichen)
                                                                  "\n" (string-append "MAYDAY " schiffsname " ICH BUCHSTABIERE "
                                                                                      (buchstabierenS schiffsname))))
    (define (notzeichen) "MAYDAY MAYDAY MAYDAY")
    (define (hierIst) (car(one-of '("DELTA ECHO" "HIER IST"))))
      (define (buchstabierenS schiffsname) (if (equal? schiffsname "SEASIDE")(string-append "SIERRA ECHO ALFA SIERRA INDIA DELTA ECHO")
                                               (string-append "ALFA MIKE INDIA ROMEO ALFA")))
    (define (peilzeichen) "ICH SENDE DEN TRÄGER -- ")
;    (define (erforderteHilfe) (if (=(random 6) 0) (string-append (erforderteHilfe))(car(one-of '("KEINE VERLETZTEN" "VIER MANN GEHEN
;    IN DIE RETTUNGSINSEL" "SCHNELLE HILFE ERFORDERLICH" "9 Mann an Bord" "Das Schiff ist 15m lang" "grüner Rumpf")))))
    (define (unterschrift schiffsname rufzeichen) (if (equal? schiffsname "SEASIDE") "SIERRA ECHO ALFA SIERRA INDIA DELTA ECHO"
                                                      "ALFA MIKE INDIA ROMEO ALFA")(buchstabierenR rufzeichen))
       (define (buchstabierenR rufzeichen) (if (equal? rufzeichen "SSDE") "SIERRA SIERRA DELTA ECHO" "ALFA MIKE ROMEO YANKEE"))

;2.3
(display (notmeldungGenerieren "SEASIDE" "SSDE" "UNGEFÄHR 10 SM NORDÖSTLICH LEUCHTTURM KIEL \n NOTFALLZEIT 1000 UTC" "SCHWERER
WASSEREINBRUCH WIR SINKEN" "KEINE VERLETZTEN \n VIER MANN GEHEN IN DIE RETTUNGSINSEL \n SCHNELLE HILFE ERFORDERLICH "))

(display (notmeldungGenerieren "AMIRA" "AMRY" "57°46'N, 006°31'E \n Notfallzeit 0640 UTC" "nach Kenterung in schwerer See, sinkt"
                               "9 Mann an Bord \n Das Schiff ist 15 m lang \n grüner Rumpf "))

;Aufgabe 3:
;3.1
; Bei der inneren Reduktion werden die Terme von innen nach außen reduziert und bei äußrerer Reduktion von außen nach innen.
( define ( hoch4 x ) ( * x x x x ) )
; innere Reduktion von ( hoch4 ( * 3 (+ 1 ( hoch4 2 ) ) ) ): Zunächst wird ( hoch4 2) als 2^4 ausgewertet. Wir erhalten dafür das Ergebnis 16.
; Dann wird dazu 1 addiert. Wir erhalten also 17. Dieses 17 wird mit 3 multipliziert und wir erhalten 51. Es wird 51^4 berechnet, was 6765201 ergibt.

; äußere Reduktion von ( hoch4 ( * 3 (+ 1 ( hoch4 2 ) ) ) ): Es wird ( hoch4 ( * 3 (+ 1 ( hoch4 2 ) ) ) ) zu
; (*( * 3 (+ 1 ( hoch4 2 ) ) ) ( * 3 (+ 1 ( hoch4 2 ) ) ) ( * 3 (+ 1 ( hoch4 2 ) ) ) ( * 3 (+ 1 ( hoch4 2 ) ) ) ausgewertet.
; Dann wird jeweils * 3 zu (+ (+ 1 ( hoch4 2 ) (+ 1 ( hoch4 2 ) (+ 1 ( hoch4 2 )) ausgewertet. + kann nicht weiter ausgewertet werden, also wird (hoch4 2)
; jeweils zu (* 2 2 2 2) ausgewertet und das wird zu 16 ausgewertet. Anschließend kann damit weiter gerechnet werden und es ergibt sich insgesamt 6765201.

;3.2
;Racket wertet normalerweise einen Ausdruck von innen nach außen aus (innere Reduktion). Für speial form expressions wird allerdings
;stattdessen der Ausdruck von außen nach innen ausgewertet.

;3.3 
( define (new-if condition? then-clause else-clause)
  (cond 
    (condition? then-clause)
    (else else-clause )))

( define ( faculty product counter max-count )
  ( new-if (> counter max-count)
    product
    ( faculty ( * counter product)
      (+ counter 1)
      max-count)))
; Bei Anwendung der Funktion Fakultät von Alyssa P. Hacker wird nach einigem Warten eine Fehlermeldung ausgegeben, in der steht, dass dem Programm der Speicher
; ausgegangen ist. Die Funktion new-if verbraucht also viel zu viel Speicher beim berechnen von 5!. Mit dem gewöhnlichen if konnte 5! aber problemlos ausgerechnet
; werden. Dies kommt dadurch, dass wegen der Auswertung "von innen" das else immer wieder ausgeführt wird ohne je abgebrochen zu werden.
; Deswegen läuft das Programm unendlich lange oder bis der Speicher reicht. 
; Daher muss insbesondere bei der Rekursiven Auswertung die äussere Reduktion verwendet werden.