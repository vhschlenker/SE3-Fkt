#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;Aufgabe 1:
;Kopfstueck ist eine lineare Rekursion, weil in jedem Rekursionschritt nur eine weitere 
;Rekursion hinzugefuegt wird. Deswegen kann es sich aber auch nicht um eine Baumrekursion handeln.
;Da es keinen geschachtelten Aufruf von kopfstueck gibt, ist kopfstueck keine geschachtelte Rekursion.
;Da kopfstueck sich selbst aufruft, handelt es sich um eine direkte Rekursion. Da kopfstueck nicht zu
;einer anderen Funktion aufgerufen wird, die von kopfstueck verwendet wird, ist kopftstueck nicht
;indiekt rekursiv.
;Es gilt also: linear, nicht baumrekursiv, nicht geschachtelt, direkt, nicht indirekt
;Endstueck ist eine lineare Rekursion, weil in jedem Rekursionschritt nur eine weitere 
;Rekursion hinzugefuegt wird. Deswegen kann es sich aber auch nicht um eine Baumrekursion handeln.
;Da es keinen geschachtelten Aufruf von endstueck gibt, ist endstueck keine geschachtelte Rekursion.
;Da endstueck sich selbst aufruft, handelt es sich um eine direkte Rekursion. Da endstueck nicht von
;einer anderen Funktion aufgerufen wird, die von endstueck verwendet wird, ist endstueck nicht
;indiekt rekursiv.
;Es gilt also: linear, nicht baumrekursiv, nicht geschachtelt, direkt, nicht indirekt
;merge ist keine lineare Rekursion, weil jeder Aufruf von merge dafr sorgt, dass merge zwei weitere
;Male aufgerufen wird. Da merge sich bei jedem Aufruf zweimal selbst aufruft, ist merge eine 
;Baumrekursion. Da nicht merge in einem Auruf von merge vorkommt, ist merge keine geschachtelte Rekursion.
;merge ist eine direkte Rekursion, weil merge sich selbst auruft. Da merge nicht von
;einer anderen Funktion aufgerufen wird, die von merge verwendet wird, ist merge nicht
;indiekt rekursiv.
;Also gilt: nicht linear, Baumrekursion, nicht geschachtelt, direkt und nicht indirekt
;merge-sort ist keine lineare Rekursion, weil merge-sort merge aufruft und dann von merge zweimal 
;benutzt wird. Es folgen also auf jeden Aufruf von merge-sort zwei weitere Aufrufe von merge-sort.
;Es handelt sich also um eine Baumrekursion. Die Rekursion ist geschachtelt, weil merge die Funktion 
;merge-sort auruft und ist aus diesem Grund auch indirekt. Da merge-sort sich nicht selbst aufruft,
;ist merge-sort nicht direkt.
;Es gilt also nicht linear, Baumrekursion, geschachtelt, nicht direkt und indrekt.

;Aufgabe 2:
;Erstellt das Bild
; width ist für die Breite
; heigth ist für die Höhe
; pines ist die Anzahl der tannen
; snowsize ist die grösse der "Schneekugeln"
(define (makeItSnow)
  (let ([width 1000]
        [height 800]
        [pines 12]
        [snowsize 30])
        (overlay-it pines (fillWithSnow width height snowsize))))

;Legt n-mal eine Tanne auf die scene
(define (overlay-it n scene [i 0])
  (if (> n i)
      (overlay-it n (drawPineOnScene scene) (+ 1 i))
      scene))
      
; Setzt zufällig eine Tanne auf die Szene
(define (drawPineOnScene scene)
  (let* (
         [width (image-width scene)]
         [height (image-width scene)]
         [y (random 1 width)]
         [x (random 1 height)]
         [seg (random 3 12)]
         [widthEight (/ width 8)])
    (place-image (pine widthEight seg) y x scene)))

; Erzeugt eine Tanne mit der Breite width
; und der Segmentanzahl segments
(define (pine width segments [segment 1])
  (if (> segment segments)
      (crop/align "center" "center" 200 width(rectangle (/ width 8) 50 "solid" "brown"))
      (overlay/xy (crop/align "center" "center" 200 width(isosceles-triangle (* width (/ segment 10)) 90 "solid" (returnRandomElementFromList pineColors))) 0 (* width (/ segment 25)) (pine width segments (+ segment 1)))))

; Erzeugt ein Bild der Grösse width und heigth
; Und der "Schneeballgrösse" snowsize
(define (fillWithSnow width height snowsize [ypos 0])
  (if (< ypos height)
      (underlay/xy (lineOfSnow width snowsize) 0 (/ snowsize 2) (fillWithSnow width height snowsize (+ ypos (/ snowsize 2))))
      (lineOfSnow width snowsize)))

; Erzeugt eine einzelne Reihe von Schnee der breite imageWidth und der "Schneeballgrösse" snowsize
(define (lineOfSnow imageWidth snowsize [ypos 0])
  (if (< (+ ypos snowsize) imageWidth)
      (overlay/xy (makeSnowball snowsize) snowsize 0 (lineOfSnow imageWidth snowsize (+ ypos snowsize)))
      (makeSnowball snowsize)))

; Erzeugt einen einzelnen Schneeball" mit der Schneegrösse snowsize
(define (makeSnowball snowsize)
  (crop 0 0 (* 2 snowsize) snowsize (circle snowsize "solid" (returnRandomElementFromList snowColors))))

(define snowColors '(Snow FloralWhite Ivory Honeydew MintCream LightCyan AliceBlue Azure GhostWhite White WhiteSmoke))
(define pineColors '(YellowGreen OliveDrab MediumForestGreen DarkGreen ForestGreen SeaGreen))
(define (returnRandomElementFromList list)
   (if (empty? list)
      '()
      (car (shuffle list))))
