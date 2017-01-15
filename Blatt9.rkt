#lang swindle
(require swindle/setf 
swindle/misc)

;Aufgabe 1
;1.1
(defclass* wissenschaftlicheVeroeffentlichung()
 (schluessel :initvalue "0000000" :accessor key :initarg :key)
 (autor :initvalue "Max Mustermann" :accessor au :initarg :au)
 (erscheinungsjahr :initvalue 0 :accessor y :initarg :y)
 (titel :initvalue "Mustertitel" :accessor t :initarg :t)
  :printer #t
)

(defclass* buch(wissenschaftlicheVeroeffentlichung)
  (verlag :initvalue "Musterverlag" :accessor vl :initarg :vl)
  (verlagsort :initvalue "Musterverlagsort" :accessor vo :initarg :vo)
  (reihe :initvalue "Musterreihe" :accessor rh :initarg :rh)
  (seriennummer :initvalue "00000000" :accessor sn :initarg :sn)
   :printer #t
  )

(defclass* sammelband(buch)
  (herausgeber :initvalue "Max Mustermann" :accessor h :initarg :h)
  (seitenangabe :initvalue "p. 0" :accessor sa :initarg :sa)
   :printer #t
  )

(defclass* zeitschriftenartikel(wissenschaftlicheVeroeffentlichung)
  (nameDerZeitschrift :initvalue "Musterzeitschrift" :accessor n :initarg :n)
  (nummerDesBandes :initvalue "0" :accessor nr :initarg :nr)
  (erscheinungsmonat :initvalue "unknown" :accessor mo :initarg :mo)
   :printer #t
  )

(define makeBuch1
 (make buch :key "0000001" :au "Nessie" :y 1790 :t "Mein Leben in Loch Ness: verfolgt als Ungeheuer"
      :vl "Minority-Verlag" :vo "Inverness" :rh "Band 1 der Reihe: Die besondere Biografie" :sn "00000001"))

(define makeSammelband1
  (make sammelband :key "0000002" :au "Prefect F." :y 1979
        :t "Mostly Harmless - some observations concerning the third planet of the solar system"
        :vl "Galactic Press" :vo "Vega-System" :rh "The Hitchhiker's Guide to the Galaxy, volume 5"
        :sn "1500 edition" :h "Adams D." :sa "p. 500"))

(define makeZeitschriftenartikel1
  (make zeitschriftenartikel :key "0000003" :au "Wells, H.G." :y 3200 :t "Zeitmaschinen leicht gemacht"
        :n "Heimwerkpraxis fuer Anfaenger" :nr "500(3)"))

;1.2
;(define cite (literaturbeitrag))

;1.3 Ergaenzungsmethoden:
;;Eine Ergaenzungsmethode ergänzt die Methode einer Oberklasse. Dies kann auf drei unterschiedliche
;;Weisen passieren: Es kann eine Vormethode (:before) ergänzt werden. Diese wird ausgeführt, bevor der Teil
;;aus der Oberklasse ausgeführt wird. Eine Nachmehode (:after) wird dementsprechend nach der urspuenglichen
;;Methode ausgefuehrt. Bei einer einhuellenden Methode (:around) wird ein Teil der ergänzten Methode ausgefuehrt.
;;Dann wird der ergänzte Teil ausgefuehrt und anschließend der Rerst von der Methode der Oberklasse ausgefuehrt.
;;Der Vorteil gegenueber eines super-calls in Java ist, dass wir einhuellende Methoden schreiben koennen.
;;Damit wir Ergaenzungsmethoden benutzen koennen, muessten wir zunaechst erstmal Methoden haben. Zudem
;;muessten wir dann jedes Attribut der Klassen ausgeben koennen, damizt wir diese ergaenzen koennen.

;Aufgabe 2:
;2.1
(defclass* Fahrzeug()
  (medium :initvalue "" :accessor me :initarg :me)
  (maximalgeschwindigkeit :initvalue 0 :accessor mg :initarg :mg)
  (tragfaehigkeit :initvalue "unbekannt" :accessor tf :initarg :tf)
  (verbrauch :initvalue 0 :accessor v :initarg :v)
  (passagierzahl :initvalue 1 :accessor pz :initarg :pz)
   :printer #t
 )
  
(defclass* Landfahrzeug(Fahrzeug)
  (medium :initvalue '(SchienenOderStrassen) :accessor me :initarg :me )
 :printer #t
) 

(defclass* Schienenfahrzeug(Landfahrzeug)
  (medium :initvalue '(Schienen) :accessor me :initarg :me)
  :printer #t
)

(defclass* Strassenfahrzeug(Landfahrzeug)
  (medium :initvalue '(Strasse) :accessor me :initarg :me)
  :printer #t
  )

(defclass* Wasserfahrzeug(Fahrzeug)
  (medium :initvalue '(Wasser) :accessor me :initarg :me)
  :printer #t
  )

(defclass* Luftfahrzeug(Fahrzeug)
    (medium :initvalue '(Luft) :accessor me :initarg :me)
  :printer #t
 )

(defclass* Amphibienfahrzeug(Landfahrzeug Wasserfahrzeug)
  :printer #t
)

(defclass* Amphibienflugzeug(Luftfahrzeug Strassenfahrzeug Wasserfahrzeug)
  :printer #t
)

(defclass* Zweiwegefahrzeug(Schienenfahrzeug Strassenfahrzeug)
  :printer #t
)

(defclass* Zeitzug(Schienenfahrzeug Luftfahrzeug)
  :printer #t
)

;2.2
;Bei den erbenden Klassen werden weitere Medien hinzugefuegt. Die Objekte der Unterklasse
;koennen auf jedem Medium fahren, auf dem auch dieObjekte der Oberklassen fahren koennen.
;Die Liste der moeglichen Medien wird also immer weiter ergaenzt.
(defgeneric medium ((Fahrzeug))
  :combination generic-append-combination)

;Die Maximalgeschwindigkeit ist die Geschwindigkeit, die ein Objekt der Klasse maximal
;erhalten kann. Das ist das Maximum der Maximalgeschwindigkeit der Klassen, von denen geerbt werden.
;Wir gehen davon aus, dass der Benutzer weiss, auf welchem Medium idiese Maximalgeschwindigeit gilt.
(defgeneric maximalgeschwindigkeit ((Fahrzeug))
  :combination generic-max-combination)

;Die Tragfaehigkeit kann durch Hinzufuegen eines neuen Mediums groesser werden, aber maximal
;so gross wie die Tragfähigkeit des neuen Mediums. 
(defgeneric tragfaehigkeit ((Fahrzeug))
  :combination generic-max-combination)

;Wir gehen davon aus, dass der Verbrauch fuer jedes weitere Medium weiter ansteigt. 
(defgeneric verbrauch ((Fahrzeug))
  :combination generic-+-combination)

;Die Passagierzahl entspricht der maximalen Passagierzahl der Objekte aus der Oberklasse.
(defgeneric passagierzahl ((Fahrzeug))
 :combination generic-max-combination)

;2.3
;Wir implementieren die Methode medium. Die generische Funktion medium arbeitet so, dass sie alle
;geerbten Elemente und die Elemente des Objekts aneinander fuegt. Es wird eine Liste mit allen
;geerbten Medien ausgegeben.
;Die Klassenpraezenenzliste gibt an, in welcher Reihenfolge die Vererbung stattfindet. Wir koennen
;an dem Amphibienflugzeug an der Reihenfolge der Elemente der Medien erkennen, dass zunächst von
;der Klasse Luftfahrzeug das Medium Luft geerbt wird. Es ist davon auszugehen, dass danach von der
;Klasse Fahrzeug geerbt wird. Da aber von der Klasse Fahrzeug nur '() geerbt wird, kann dies nicht
;abgelesen werden. Anschliessend wird von der Klasse Strassenfahrzeug das Medium Strasse geerbt.
;Es folgt die Vererbung von dem Element SchienenOderStrasse aus der Klasse Landfahrzeug. Zuletzt
;wird noch von der Klasse Wasserfahrzeug das Medium Wasser geerbt.
(defmethod medium ((f Fahrzeug)) '())

(defmethod medium ((lf Landfahrzeug)) '(SchienenOderStrassen))

(defmethod medium ((wf Wasserfahrzeug)) '(Wasser))

(defmethod medium ((luf Luftfahrzeug)) '(Luft))

(defmethod medium ((stf Strassenfahrzeug))'(Strasse))

(defmethod medium ((scf Schienenfahrzeug))'(Schiene))

(display (make Amphibienfahrzeug)) 
(display (make Amphibienflugzeug))
(display (make Zweiwegefahrzeug))
(display (make Zeitzug))
(display (medium (make Amphibienfahrzeug)))
(display (medium (make Amphibienflugzeug)))
(display (medium (make Zweiwegefahrzeug)))
(display (medium (make Zeitzug)))



  


  
  

