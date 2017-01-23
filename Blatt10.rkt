#lang racket
;Aufgabe 1: Sudoku:
;;1.1
(define spiel #(0 0 0 0 0 9 0 7 0
                0 0 0 0 8 2 0 5 0
                3 2 7 0 0 0 0 4 0
                0 1 6 0 4 0 0 0 0
                0 5 0 0 0 0 3 0 0
                0 0 0 0 9 0 7 0 0
                0 0 0 6 0 0 0 0 5
                8 0 2 0 0 0 0 0 0
                0 0 4 2 0 0 0 0 8))

;;;1.1.1. Hilfsfunkktion index
(define (xy->index x y)
  (+ (* 9 y)x))

;;;1.1.2. Zugriff auf Indizes, Spalte, Quadrant
(define (buildListTo n)
  (build-list n values))
(define (zeile->indizes zeile)
  (map (lambda (index) (xy->index index zeile)) (buildListTo 9)))
(define (spalte->indizes spalte)
  (map (lambda (index) (xy->index spalte index)) (buildListTo 9)))

(define qs '((0 0)(1 3)(2 6)(3 27)(4 30)(5 33)(6 54)(7 57)(8 60)))
(define (quadrant->indizes q)
  (let ([s (cadr(assoc q qs))])
  (list s (+ 1 s)(+ 2 s)(+ 9 s)(+ 10 s)(+ 11 s)(+ 18 s)(+ 19 s)(+ 20 s))))


;;;1.1.3. Einträge des Spielzustands ermitteln
(define (spiel->eintraege i)
  (map (curry list-ref (vector->list spiel)) (quadrant->indizes i)))

;;;1.1.4. konsistent bzw. geloest?
(define (ungleich? i1 i2)
  (if (or (= (list-ref (vector->list spiel) i1) 0)(not (= (list-ref (vector->list spiel) i1)
         (list-ref (vector->list spiel) i2)))(equal? i1 i2))#t #f))
(define (ungleichSpalte? s)
  (not (check-duplicates (filter positive? (map (curry list-ref (vector->list spiel)) (spalte->indizes s))))))
(define (ungleichZeile? z)
  (not (check-duplicates (filter positive? (map (curry list-ref (vector->list spiel)) (zeile->indizes z))))))
(define (ungleichQuadrant? q)
  (not (check-duplicates (filter positive? (map (curry list-ref (vector->list spiel)) (quadrant->indizes q))))))
(define (ungleichAlleSpalten?)
  (empty? (remove* '(#t) (map ungleichSpalte? '(0 1 2 3 4 5 6 7 8)))))
(define (ungleichAlleZeilen?)
  (empty? (remove* '(#t) (map ungleichZeile? '(0 1 2 3 4 5 6 7 8)))))
(define (ungleichAlleQuadranten?)
  (empty? (remove* '(#t) (map ungleichQuadrant? '(0 1 2 3 4 5 6 7 8)))))
(define (spiel-konsistent? spiel)
  (and (ungleichAlleSpalten?)(ungleichAlleZeilen?)(ungleichAlleQuadranten?)))

(define (spiel-geloest? spiel)
  (equal? (vector->list spiel)(remove* '(0) (vector->list spiel))))


;;1.2
;;;1.2.1. Annotieren des Spielfeldes
(define qs2 (list (list 0 0)(list 1 0)(list 2 0)(list 3 1)(list 4 1)(list 5 1)(list 6 2)(list 7 2)(list 8 2)
                 (list 9 0)(list 10 0)(list 11 0)(list 12 1)(list 13 1)(list 14 1)(list 15 2)(list 16 2)(list 17 2)
                 (list 18 0)(list 19 0)(list 20 0)(list 21 1)(list 22 1)(list 23 1)(list 24 2)(list 25 2)(list 26 2)
                 (list 27 3)(list 28 3)(list 29 3)(list 30 4)(list 31 4)(list 32 4)(list 33 5)(list 34 5)(list 35 5)
                 (list 36 3)(list 37 3)(list 38 3)(list 39 4)(list 40 4)(list 41 4)(list 42 5)(list 43 5)(list 44 5)
                 (list 45 3)(list 46 3)(list 47 3)(list 48 4)(list 49 4)(list 50 4)(list 51 5)(list 52 5)(list 53 5)
                 (list 54 6)(list 55 6)(list 56 6)(list 57 7)(list 58 7)(list 59 7)(list 60 8)(list 61 8)(list 62 8)
                 (list 63 6)(list 64 6)(list 65 6)(list 66 7)(list 67 7)(list 68 7)(list 69 8)(list 70 8)(list 71 8)
                 (list 72 6)(list 73 6)(list 74 6)(list 75 7)(list 76 7)(list 77 7)(list 78 8)(list 79 8)(list 80 8)))
(define (kannNichtGesetztWerden? pos zahl)
  (if(and (if(empty?(filter (curry equal? zahl) (map(curry list-ref (vector->list spiel))(zeile->indizes (quotient pos 9)))))#f #t)
         (if(empty?(filter (curry equal? zahl) (map(curry list-ref (vector->list spiel))(spalte->indizes (modulo pos 9)))))#f #t)
         (if(empty?(filter (curry equal? zahl) (map(curry list-ref (vector->list spiel))(quadrant->indizes (cadr(assoc pos qs2))))))#f #t)
         (if(= (list-ref (vector->list spiel) pos)0) #f #t))
         #f #t))
(define (getPositionen zahl)
  (filter (curry equal? zahl)(vector->list spiel)))
(define (markiere-ausschluss spiel zahl)
  (display(let ([c (vector-copy spiel)])(if(kannNichtGesetztWerden? 0 zahl)(vector-set! c 0 zahl) #\X))))
    
; Aufgabe 2
; Teilaufgabe 1
(define (teilaufgabe1)
  (values
   ; (- 2 3) -> -1 (min 2 -1) -> -1 (max -1) -> -1
   (max (min 2 (- 2 3)))
   ; Durch die weak clause wird die Auswertung abgebrochen
   ; Daher '(+ -2 2)
   `(+ ,(- 2 4) 2)
   ; 'Alle
   (car '( Alle meine Entchen))
   ; '((dem See))
   (cdr '( (schwimmen auf) (dem See)))
   ; '((Listen) sind einfach )
   (cons '(Listen ) '( sind einfach ))
   ; '( Paare . auch)
   (cons 'Paare 'auch)
   ; #t
   (equal?
    (list 'Java 'Racket 'Prolog )
    '(Java Racket Prolog))
   ; #f
    (eq?
     (list 'Java 'Racket 'Prolog )
     (cons 'Racket '(Prolog Java )))
    ; '(3 6 9)
    (map
     (lambda ( x ) (+ x x x ))
     '( 1 2 3 ))
    ; '( 2 4)
    (filter even? '(1 2 3 4 5))
    ; 2
    ((curry min 7) 2)
    ; #t
    ((curry = 2) 2)
    ))
; Teilaufgabe 2
(define ∗a∗ 10 )
(define ∗b∗ '∗a∗ )
(define ( merke x) (lambda () x ))
; Eine schliessende Klammer zu wenig, wurde hinzugefügt
(define (test x )
  (let ((x (+ x ∗a∗)))
    (+ x 2)))
(define (teilaufgabe2)
  (values
   ; 10
   ∗a∗
   ; Addition mit Symbol, d.h contract violation
   ;(+ ∗a∗ ∗b∗)
   ; 20
   (+ (eval ∗a∗) (eval ∗b∗))
   ; #f da (and #f #f)
   (and (> ∗a∗ 10) (> ∗b∗ 3))
   ; Durch null teilen?
   ; Und Racket dann so: nö.
   ;(or (> ∗a∗ 10) (/ ∗a∗ 0))
   ; Wir kriegen bei (merke 3) das procedure (3) zurück
   ; Daher keine addition
   (+ 2 (merke 3))
   ; Durch die Klammer um das procedure was return wird,
   ; wird es ausgeführt. D.h 5
   (+ 2 ((merke 3)))
   ; Nochmals, fehlende Klammer bei (test)
   ; Wegen Überschattung 16
   ( test 4)
   ))
; Teilaufgabe 3
(define (3a)
  (* 3 (+ 4 5) 6))
(define (3b x)
  (sqrt (- 1 (expt (sin x) 2))))
; Teilaufgabe 4
(define (c a b)
  (sqrt (+ (expt a 2) (expt b 2))))
(define (mytan alpha)
  (if (and (< (- (/ pi 2)) alpha) (< alpha (/ pi 2)))
    (/ (sin alpha) (3b alpha))
    'error
))
; Teilaufgabe 5
(define (5a)
  (- (+ 1 (/ 4 2)) 1))
(define (5b)
  (/ (- 2 (/ (+ 1 3)(* (+ 3 2) 3)(sqrt 3)))))
; Teilaufabe 6
; ((1 + 2 + 3) * (2 - 3 - (2 / 3)))

; Teilaufgabe 7
; (a)
; Einfach gesagt:
; Bei der äusseren Reduktion wird von aussen nach innen ausgewertet,
; Bei der inneren Reduktion von innen nach aussen.
; (b)
; Das macht bei Operationen wie Additionen etc. zwar keinen Unterschied,
; Aber bei Operationen mit Bedienungen, wo eine Operation bei erfüllen
; der Bedingung X nicht mehr ausgeführt werden soll einen gehörigen unterschied.
; Denn bei der inneren Reduktion kann es deswegen zu Endlosschleifen kommen.

; Teilaufgabe 8
; endrekusrion -> kein nachklappern
(define (laengen1 xss)
  (if (empty? xss)
      '()
      (cons (length (car xss)) (laengen1 (cdr xss)))))
(define (laengen2 xss [out '()])
  (if (empty? xss)
      (reverse out)
      (laengen2 (cdr xss) (cons (length (car xss)) out))))
(define (test8)
  (values
   (laengen1 '((1 2 3 4) (Auto Bus) (()) ()))
   (laengen2 '((1 2 3 4) (Auto Bus) (()) ()))
   ))  