#lang racket

;;; 1.

;; kopfstueck: linear rekursiv, direkt rekursiv
;Es gibt nur einen rekursiven Aufruf pro Rekursionsschritt, daher ist die Funktion linear rekursiv.
;Da sie sich selber aufruft ist sie direkt rekursiv.

;; endstueck: linear rekursiv, (endrekursiv), direkt rekursiv
;Auch hier gibt es nur einen rekursiven Aufruf pro Schritt.
;Da auch diese Funktion sich selber aufruft ist auch sie direkt rekursiv.

;; merge: linear rekursiv, direkt rekursiv
;Durch die if-bedingung wird in jedem Rekursionsschritt höchstens eine weitere merge-funktion aufgerufen, es handelt sich also um lineare rekursion.
;Da merge sich selber wieder aufruft ist es direkt rekursiv.

;; merge-sort: baumrekursiv,  geschachtelte rekursion, indirekt rekursiv (?)
;Da es in jedem rekursiven Schritt mehrere rekursive Aufrufe gibt, ist die Funktion nicht linear rekursiv.
;Der Aufruf von merge benutzt rekursive Funktionen, somit ist in merge-sort eine geschachtelte Rekursion vorhanden.
;Da in jedem Aufruf von merge-sort merge aufgerufen wird und dafür zwei mal merge-sort aufgerufen wird, handelt es sich um Baumrekursion.
;Begründung in/direkt rekursiv...