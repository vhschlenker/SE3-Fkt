#lang racket

;;allgemein rekursiv

(define (produkt list x)
  (if (empty? list) '() (cons (* x (car list)) (produkt (cdr list) x))))

;;endrekursiv

(define (produkt2 list x)
  (letrec ([innere (lambda (ls1 ls2)
                     (if(empty? ls1) ls2 (innere (cdr ls1) (cons (* x (car ls1)) ls2))))])
    (reverse (innere list '()))))

;;mittels Funktion höherer Ordnung

(define (produkt3 list x)
  (map ((curry *) x) list))