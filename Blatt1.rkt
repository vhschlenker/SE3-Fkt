#lang racket
;Aufgabe 1.1:
;Umrechung von Grad in Radian:
(define (degrees->Radians d)
  (* d (/ 17453293 1000000000)))

;Umrechnung von Radian in Grad:
(define (radians->Degrees r)
  (* r (/ 5729577951 100000000)))

;Aufgabe 1.2:
;Umrechung von Cosinus in Tangens:
;Zuerst wird der Sinus ausgrechnet:
(define (cosInSin cosinus)
  (sqrt(- 1 (* cosinus cosinus))))
;Ausrechnen des Tangens:
(define (cosSinInTangens cosinus)
   (/ (cosInSin cosinus)cosinus)) 

;Definition von my-acos:
 (define (my-arcos cosinus)
   (radians->Degrees(atan(cosSinInTangens cosinus))))

;Aufgabe 1.3:
;Umrechnung von Seemeilen in Kilometer:
(define (nm->km seemeilen)
  (* seemeilen (/ 1852 1000)))

;Aufgabe 2.1:
;zunächst die Formel für die Großkreisberechnung (gkb) mit Geographischer Breite A (phiA),
;Geographischer Breite B (phiB), der geographischen Länge von A(lambdaA) und der geographischen
;Länge von B (lambdaB).
(define (gkb phiA phiB lambdaA lambdaB )
  (+(* (sin (degrees->Radians phiA))(sin (degrees->Radians phiB))) 
  (* (cos (degrees->Radians phiA))(cos (degrees->Radians phiB))(cos (degrees->Radians (abs (- lambdaA lambdaB)))))))
;Definition von DistanzAB:
(define (distanzAB phiA phiB lambdaA lambdaB)
  (nm->km(*(my-arcos(gkb phiA phiB lambdaA lambdaB ))60)))
;Entfernung von Oslo nach Hongkong:
(distanzAB  59.93 22.20 10.75 114.10)
;Die Entfernung beträgt 8589.41 km.
;Entfernung von San Francisco nach Honolulu:
(distanzAB 37.75 21.32 -122.45 -157.83)
;Entfernung von 3844.688 km.
;Entfernung von der Osterinsel nach Lima:
(distanzAB -27.10 -12.10 -109.4 -77.05)
;Entfernung von 3757.622 km.

;Aufgabe 2.2:
;Zunächst die Formel zur Berechnung von cos alpha: 
(define (zielRichtung phiB phiA lambdaA lambdaB)
  (/(- (sin phiB) (* (gkb phiA phiB lambdaA lambdaB) (sin phiA)))(*(cos phiA)(cosInSin (gkb phiA phiB lambdaA lambdaB)))))
;Berechnung des Winkels und Korrektur:
(define (kursrichtung phiB phiA lambdaA lambdaB )
  (if ( <  0 (my-arcos(zielRichtung phiB phiA lambdaA lambdaB))  180) 'East  'West )) 

;Aufgabe 2.3:
;2.3.1:
(define (Grad->Himmelsrichtung grad)
  (cond [(=  grad 0) 'N] [(= grad 22.5) 'NNO] [(= grad 45) 'NO] [(= grad 67.5) 'ONO]
        [(= grad 90) 'O] [(= grad 112.5) 'OSO] [(= grad 135) 'SO] [(= grad 157.5) 'SSO]
        [(= grad 180) 'S] [(= grad 202.5) 'SSW] [(= grad 225) 'SW] [(= grad 247.5) 'WSW]
        [(= grad 270) 'W] [(= grad 292.5) 'WNW] [(= grad 315) 'NW] [(= grad 337.5) 'NNW]))
;2.3.2
(define (Himmelsrichtung->Grad himmelsrichtung)
    (cond [(equal? himmelsrichtung 'N) 0] [(equal? himmelsrichtung 'NNO) 22.5]  [(equal? himmelsrichtung 'NO) 45] [(equal? himmelsrichtung 'ONO) 67.5]
          [(equal? himmelsrichtung 'O) 90] [(equal? himmelsrichtung 'OSO) 112.5]  [(equal? himmelsrichtung 'SO) 135] [(equal? himmelsrichtung 'SSO) 157.5]
          [(equal? himmelsrichtung 'S) 180] [(equal? himmelsrichtung 'SSW) 202.5]  [(equal? himmelsrichtung 'SW) 225] [(equal? himmelsrichtung 'WSW) 247.5]
          [(equal? himmelsrichtung 'W) 270] [(equal? himmelsrichtung 'WNW) 292.5]  [(equal? himmelsrichtung 'NW) 315] [(equal? himmelsrichtung 'NNW) 337.5]))
        
        
        

  




  
  