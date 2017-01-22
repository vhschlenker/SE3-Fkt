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
(define (zeile->indizes y)
  (list (xy->index 0 y)(xy->index 1 y)(xy->index 2 y)(xy->index 3 y)(xy->index 4 y)
        (xy->index 5 y)(xy->index 6 y)(xy->index 7 y)(xy->index 8 y)))
(define (spalte->indizes x)
  (list (xy->index x 0)(xy->index x 1)(xy->index x 2)(xy->index x 3)(xy->index x 4)
        (xy->index x 5)(xy->index x 6)(xy->index x 7)(xy->index x 8)))
(define qs (list (list 0 0)(list 1 3)(list 2 6)(list 3 27)(list 4 30)(list 5 33)(list 6 54)(list 7 57)(list 8 60)))
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
  (if (and (ungleichAlleSpalten?)(ungleichAlleZeilen?)(ungleichAlleQuadranten?))#t #f))

(define (geloest? spiel)
  (if (equal? (vector->list spiel)(remove* '(0) (vector->list spiel)))#t #f))


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
    










  