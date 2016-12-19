#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;Aufgabe 1

; allgemein Rekursiv
(define (produkt1 list zahl)
  (if (empty? list)
      '()
      (cons (* (car list) zahl)(produkt1 (cdr list) zahl))))

; Endrekursiv 1
(define (produkt2 list zahl)
  (letrec ([innere (lambda(list ergebnis)
                     (if (empty? list)
                         (reverse ergebnis)
                         (innere(cdr list)(cons (* (car list) zahl) ergebnis ))))])
  (innere list '()))
  )

; Endrekursiv 2
(define (produkt3 liste multi [result '()])
  (if (empty? liste)
      (reverse result)
      (produkt3 (cdr liste) multi (cons (* multi (car liste)) result))))

; Höhere Ordnung 1
(define (produkt4 list zahl)
  (map (lambda(number)
         (* zahl number))list))

; Höhere Ordnung 2
(define (produkt5 liste multi)
  (map (curry * multi) liste))

(define (testAll)
  (let ([liste '(2 4 3)]
        [multi 3])
    (values
     (produkt1 liste multi)
     (produkt2 liste multi)
     (produkt3 liste multi)
     (produkt4 liste multi)
     (produkt5 liste multi))))

; Aufgabe 2.1
; Die Kodierung des Zustandes könnte als Liste von Booleans in einer liste erfolgen.
; Die Zahlen in den eckigen Klammern stehen für den Index.
;    - [0]
; | [1] | [2]
;    - [3]
; | [4] | [5]
;    - [6]
(define numbers
 '(
   (0 . (#t #t #t #f #t #t #t))
   (1 . (#f #f #t #f #f #t #f))
   (2 . (#t #f #t #t #t #f #t))
   (3 . (#t #f #t #t #f #t #t))
   (4 . (#f #t #t #t #f #t #f))
   (5 . (#t #t #f #t #f #t #t))
   (6 . (#t #t #f #t #t #t #t))
   (7 . (#t #f #t #f #f #t #f))
   (8 . (#t #t #t #t #t #t #t))
   (9 . (#t #t #t #t #f #t #t))
   ("off" . (#f #f #f #f #f #f #f))
   ("-" . (#f #f #f #t #f #f #f))
   ))

; Datenstruktur für ein Rechteck
; Jedes Segment ist eine Liste mit Orientierung - Xpos - Ypos
; Rechteckgrösse 200*100, Segmentgrösse 80*10
; Die einzelne Segmentgrösse ist nicht exakt eingehalten
; aus Designaspekten
(define rectangleList
  '(
    (v 15 5)
    (h 5 15)
    (h 85 15)
    (v 15 95)
    (h 5 105)
    (h 85 105)
    (v 15 185)))


; Aufgabe 2.2
; Nimmt eine Nummer von 0-9 an und stellt sie als Zahl dar.
(define (sevenSegmentNummer number)
  (drawSevenSegmentNummer (returnStruct number) rectangleList))

; Zeichnet eine sieben Segmentanzeige mit Hilfe
; der Liste der leuchtenden Dioden und ihrer Positionen
(define (drawSevenSegmentNummer boolList rectangleList)
  (if (empty? boolList)
      (rectangle 100 200 "solid" "Black")
      (let ([rectangleProps (car rectangleList)])
        (underlay/xy (drawSevenSegmentNummer (cdr boolList) (cdr rectangleList)) (cadr rectangleProps) (caddr rectangleProps) (returnRectangle (car rectangleProps) (car boolList))))))

; Gibt ein einzelnes Segment zurück mit der Orientierung 'v für vertikal,
; alles andere für horizontal und einem boolean für an oder aus
(define (returnRectangle orientation enabled)
  (let ([color (if enabled "Red" "DimGray")])
    (if (equal? orientation 'v)
        (rectangle 70 10 "solid" color)
        (rectangle 10 80 "solid" color))))

; Gibt die richtige booleanliste von der Zahlenliste zurück
(define (returnStruct number)
  (cdr (assoc number numbers)))

; Aufgabe 2.3
; Animiert die Hauptaufgabe
(define (animate7Segment)
  (animate zeige-7segment))
; Zeigt eine sieben Segement anzeige für eine Zahl ticks t
; die 1 in der Funktion steht dafür, dass nur ein Sgement hier gezeigt werden soll
(define (zeige-7segment t)
  (zeige-x-7segment 1 (ticks->seconds t)))

; Wandelt Ticks in Sekunden um.
(define (ticks->seconds t)
  (quotient t 28))

; Aufgabe 2.4
; Zeigt eine sieben Segmentanzeige mit x vielen Segmenten und zeigt die Zahl t
; x sollte grösser gelich eins sein, und t wird in Base 10 dargestellt (0-9)
(define (zeige-x-7segment x t )
  (if (= x 1)
      (sevenSegmentNummer (modulo t 10))
      (beside (sevenSegmentNummer (quotient (modulo t (expt 10 x)) (expt 10 (sub1 x)))) (zeige-x-7segment (- x 1) t))))

; Wie zeige-x-7segment nur ein Minus wird in einem zusätzlichen Segemnt vorangestellt
(define (zeige-x-7segment-minus x t)
  (beside (sevenSegmentNummer "-") (zeige-x-7segment x (abs t))))

; Zeigt x-viele deaktivierte Segmente
(define (zeige-x-7segment-off x)
  (if (= x 1)
      (sevenSegmentNummer "off")
      (beside (sevenSegmentNummer "off") (zeige-x-7segment-off (- x 1)))))

; Animiert die Hauptaufgabe
(define (animateZeigeDauer)
  (animate zeige-dauer))
; Wandelt ticks in angezeigte Sekunden um
; Sekunden springen bei 59 auf 00, ebenso die Minuten
; Stunden laufen bis 99 und springen auf 00 um
(define (zeige-dauer t)
  (let* ([secondsRunning (ticks->seconds t)]
         [seconds (modulo secondsRunning 60)]
         [minutes (modulo (quotient secondsRunning 60) 60)]
         [hours (quotient secondsRunning (* 60 60))])
    ; Hours Segment
    (beside (zeige-x-7segment 2 hours)
            ; Seperator Segment
            (returnSeperator)
            ; Minutes Segment
            (zeige-x-7segment 2 minutes)
            ; Seperator Segment
            (returnSeperator)
            ; Seconds Segment
            (zeige-x-7segment 2 seconds))))

; Erzeugt den Seperator
(define (returnSeperator)
  (underlay/xy
   (underlay/xy
    (rectangle 100 200 'solid 'black)
    40 70
    (rectangle 20 20 'solid 'red))
   40 140
   (rectangle 20 20 'solid 'red)))

; Aufgabe 2.5
; Animiert die Hauptaufgabe
(define (testTimer)
  (animate (curry zeige-timer 120)))
; Zeigt einen Timer der seconds lange läuft bei 28 ticks pro sekunde
; Stellt 3 Segmente dar
; Bei Ablauf der Zeit blinkt die Anzeige und es werden die abgelaufenen Sekunden
; seit Timerende mit einem Minus davor gezeigt
(define (zeige-timer seconds t)
  (let ([remainingSeconds (- seconds (ticks->seconds t))]
        [segments 3])
    (if (< remainingSeconds 0)
        (if (odd? (quotient t 14))
             (zeige-x-7segment-minus (sub1 segments) remainingSeconds)
             (zeige-x-7segment-off segments))
        (zeige-x-7segment segments remainingSeconds))))