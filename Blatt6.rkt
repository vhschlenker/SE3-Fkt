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
(define baum1 (above/align
"center"
(star 20 "solid" "gold")
(ellipse 20 10 "solid" "green")
(ellipse 40 25 "solid" "darkgreen")
(ellipse 65 30 "solid" "olivedrab")
(ellipse 90 40 "solid" "darkgreen")
(rectangle 20 30 "solid" "brown")
)) 
(define kerze (above/align
"center"
(ellipse 10 20 "solid" "orange")
(rectangle 2 10 "solid" "black")
(triangle 30 "solid" "red")
(rectangle 30 100 "solid" "red")))


;(define anzahl 10)
(define (erzeugeBaeumeR anzahl) 
 (if (= anzahl 0) (underlay/xy (rectangle 600 600 0 "darkblue") 350 200 kerze)
(underlay/xy (erzeugeBaeumeR (- anzahl 1)) (+ 500(* anzahl 10)) (+ 200(* anzahl 10)) baum1)
))
(define (erzeugeBaeumeL anzahl) 
 (if (= anzahl 0) (underlay/xy (rectangle 600 600 0 "darkblue") 250 200 kerze)
(underlay/xy (erzeugeBaeumeL (- anzahl 1)) (- 100(* anzahl 10)) (+ 200(* anzahl 10)) baum1)
))

(define (create-Weihnacht)
  (display(underlay/xy (underlay/xy (rectangle 1000 800 "solid" "darkblue") 200 0 (erzeugeBaeumeL 15)) 200 0 (erzeugeBaeumeR 15)) )) 
(create-Weihnacht)

(define (lineOfSnow imageWidth singleCircleWidth [ypos 0])
  (if (< ypos imageWidth)
      (overlay/offset (makeSnowball singleCircleWidth) singleCircleWidth 0 (lineOfSnow imageWidth singleCircleWidth (+ ypos singleCircleWidth)))
      (makeSnowball singleCircleWidth)))

(define snowColors '(Snow FloralWhite Ivory Honeydew MintCream LightCyan AliceBlue Azure GhostWhite White WhiteSmoke))
(define (returnRandomElementFromList list)
   (if (empty? list)
      '()
      (car (shuffle list))))
(define (makeSnowball width)
  (crop 0 0 (* 2 width) width (circle width "solid" (returnRandomElementFromList snowColors))))
