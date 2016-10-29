#lang racket
; 1 Konversionsfunktionen
; 1.1 Bogenmaß und Grad
(define (degrees->radians x)
  (* x (/ pi 180)))
(define (radians->degrees x)
  (* x (/ 180 pi)))
; 1.2 Umkehrfunktion acos
; Racket works with radians instead of degree
(define (my-acos x)
  (atan (/ (sqrt (- 1 (expt x 2))) x)))
; 1.3 Kilometer und Seemeilen
(define (nm->km x)
  (* x 1.852))
; 2.1 Großkreisentfernung
; Eingabe in Grad
( define (distanzAB breiteA breiteB laengeA laengeB)
   (let (
         [breiteARad (degrees->radians breiteA)]
         [breiteBRad (degrees->radians breiteB)]
         [langeDeltaRad (degrees->radians (abs (- laengeA laengeB)))]
         )
   (* 60
      (radians->degrees
       (my-acos
        (+
         (* (sin breiteARad)
            (sin breiteBRad))
         (* (cos breiteARad)
            (cos breiteBRad)
            (cos langeDeltaRad))))))))
; Entfernung Oslo <-> Honkong
; 8595.2km / 4641sm
(define (OsloHongkong)
  (distanzAB 59.93 22.20 10.75 114.10))
; Entfernung San Francisco <-> Honolulu
; 3847.3km / 2077.4sm
(define (SanFranciscoHonolulu)
  (distanzAB 37.75 21.32 122.45 157.83))
; Entfernung Osterinsel <-> Lima
; 3760.2km / 2030.3sm
(define (OsterinselLima)
  (distanzAB 27.1 12.1 109.4 77.05))
; 2.3 Himmelsrichtungen
(define (Grad->Himmelsrichtung grad)
  (cond
    [(< grad 0) "Min. 0 Degree"]
    [(< grad 22.5) 'N]
    [(< grad 45) 'NNE]
    [(< grad 67.5) 'NE]
    [(< grad 90) 'ENE]
    [(< grad 112.5) 'E]
    [(< grad 135) 'ESE]
    [(< grad 157.5) 'SE]
    [(< grad 180) 'SSE]
    [(< grad 202.5) 'S]
    [(< grad 225) 'SSW]
    [(< grad 247.5) 'SW]
    [(< grad 270) 'WSW]
    [(< grad 292.5) 'W]
    [(< grad 315) 'WNW]
    [(< grad 337.5) 'NW]
    [(< grad 360) 'NNW]
    [ else "Max. 359 Degree"]))

; 2.3 Himmelsrichtungen
(define (Himmelsrichtung->Grad himmelsrichtung)
  (cond
    [(equal? himmelsrichtung 'N) 0]
    [(equal? himmelsrichtung 'NNE) 22.5]
    [(equal? himmelsrichtung 'NE) 45]
    [(equal? himmelsrichtung 'ENE) 67.5]
    [(equal? himmelsrichtung 'E) 90]
    [(equal? himmelsrichtung 'ESE) 112.5]
    [(equal? himmelsrichtung 'SE) 135]
    [(equal? himmelsrichtung 'SSE) 157.5]
    [(equal? himmelsrichtung 'S) 180]
    [(equal? himmelsrichtung 'SSW) 202.5]
    [(equal? himmelsrichtung 'SW) 225]
    [(equal? himmelsrichtung 'WSW) 247.5]
    [(equal? himmelsrichtung 'W) 270]
    [(equal? himmelsrichtung 'WNW) 292.5]
    [(equal? himmelsrichtung 'NW) 315]
    [(equal? himmelsrichtung 'NNW) 337.5]
    [ else "No correct input"]))