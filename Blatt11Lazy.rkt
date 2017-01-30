#lang lazy
; Erzeugt einen Strom natürlicher Zahlen, angefangen mit n
(define (natsAbN n)
  (cons n (natsAbN (+ n 1))))

; Prüft, ob eine Zahl glatt durch eine andere teilbar ist
(define (devisibleBy? num divider)
  (= 0 (remainder num divider)))

; Wählt basierend auf der num aus, ob flipflap, flip oder flap ausgegeben werden soll.
; Sonst soll die Nummer wieder gegeben werden.
; Die erste Bedingung (not integer?) ist nötig, da der Eingabestream auch schon bearbeitet enthält.
(define (chooseAction num)
  (cond
   [(not (integer? num)) num]
   [(and (devisibleBy? num 3)(devisibleBy? num 5)) 'flipflap]
   [(devisibleBy? num 3) 'flip]
   [(devisibleBy? num 5) 'flap]
   [else num]))

; Siebt den Stream mit map auf jedem ELemnt
(define (sieve stream)
  (cons
   (car stream) (map
                  (lambda (num)
                    (chooseAction num))
                  (sieve (cdr stream)))))

; Definiert unsern stream mit dem Sieb
(define fliporflipflap (sieve (natsAbN 1)))

; Die Testfunktion, die n-viele ELemente aus dem Stream entnimmt
(define (test n)
  (!! (take n fliporflipflap)))