;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname praktische-info-uebung-1) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; -----------
; Aufgabe 1
; -----------

; Nimmt die Seitenlänge "side-length" eines Quadrats als Parameter
; und gibt den Umfang des Quadrats aus
(: square-circumference (real -> real))
(define square-circumference
  (lambda (side-length)
    (* 4 side-length)
    )
  )

; Wendet den Satz des Pythagoras auf die Parameter a und b an
; und gibt den Wert von c aus (a^2 + b^2 = c^2)
(: pythagoras (real real -> real))
(define pythagoras
  (lambda (a b)
    (sqrt
     (+
      (* a a) (* b b)
      )
     )
    )
  )

; Berechnet die Kantenlänge einer Kante zur Spitze eiener Pyramide basierend auf der Seitenlänge "side-length"
; und der höhe der Pyramide "height"
(: pyramid-one-edge-length (real real -> real))
(define pyramid-one-edge-length
  (lambda (side-length height)
    (pythagoras ; Satz des Pythagoras aus der Hälfte der Seitenlänge und der Distanz zwischen einer Seite der Grundfläche bis zur Spitze
     (/ side-length 2)
     (pythagoras ; Berechnung der Distanz zwischen Grundfläche und Spitze
      (/ side-length 2) height)
     )
    )
  )

; Berechnet die Kantenlänge einer Pyramide mit quadratischer Grundfläche basierend auf der Seitenlänge der Grundfläche "side-length
; und der Höhe der Pyramide "height"
(: pyramid-edge-length (real real -> real))
(define pyramid-edge-length
  (lambda (side-length height)
    (+
     (square-circumference side-length)
     (* (pyramid-one-edge-length side-length height) 4)
     )
    )
  )

; -----------
; Aufgabe 2
; -----------

; Rechnet einen Celsiuswert (temperature) in Fahrenheit um
(: celsius->fahrenheit (real -> real))
(define celsius->fahrenheit
  (lambda (temperature)
    (+
     32
     (*
      (/ 9 5) temperature)
     )
    )
  )

; Rechnet einen Fahrenheitwert (temperature) in Celsius um
(: fahrenheit->celsius (real -> real))
(define fahrenheit->celsius
  (lambda (temperature)
    (/
     (- temperature 32)
     1.8
     )
    )
  )

; Testet, ob zwei funktionen (func1, func2), die jeweils sowohl eine Zahl als Parameter als auch als Ergebnis haben,
; für eine Testzahl "number" invers sind 
(: number-inversion-test (real (real -> real) (real -> real) -> boolean))
(define number-inversion-test
  (lambda (number func1 func2)
    (=
     (func1 (func2 number))
     (func2 (func1 number))
     )
    )
  )

(number-inversion-test 0 celsius->fahrenheit fahrenheit->celsius)
(number-inversion-test 10 celsius->fahrenheit fahrenheit->celsius)
(number-inversion-test -10 celsius->fahrenheit fahrenheit->celsius)
(number-inversion-test 5 celsius->fahrenheit fahrenheit->celsius)
(number-inversion-test 1 celsius->fahrenheit fahrenheit->celsius)

; -----------
; Aufgabe 3
; -----------

; verknüpft den Input mit "E3" und multipliziert so den Input mit 10^3, außerdem wird das Ergebnis negiert
;(: transform (natural -> natural)) ; Signature Violation, deswegen Signatur auskommentiert
(define transform
  (lambda (x)
    (string->number (string-append "-" (number->string x) "E3"))))

(transform 0) ; 0
(transform 10) ; -10000, Signature violation
(transform 999) ; -999000, Signature violation
(transform 1) ; -1000, Signature violation

; transformiert den Input, indem er mit 10^3 multipliziert und negiert wird
(: better-transform (real -> real))
(define better-transform
  (lambda (x)
    (-
     (* x (expt 10 3))
     )
    )
  )

(better-transform 0) ; 0
(better-transform 10) ; -10000
(better-transform 999) ; -999000
(better-transform 1) ; -1000
(better-transform 0.5) ; -500
(better-transform 0.00005) ; -0.05
(better-transform -3) ; 3000


; transformiert den Input "x", indem er mit einer n-ten Potenz von 10 multipliziert und negiert wird
(: even-better-transform (real integer -> real))
(define even-better-transform
  (lambda (x n)
    (-
     (* x (expt 10 n))
     )
    )
  )

(even-better-transform 0 3) ; 0
(even-better-transform 10 3) ; -10000
(even-better-transform 999 3) ; -999000
(even-better-transform 1 3) ; -1000
(even-better-transform 0.5 3) ; -500
(even-better-transform 0.00005 3) ; -0.05
(even-better-transform -3 3) ; 3000
(even-better-transform -3 0) ; 3
(even-better-transform -3 100) ; 30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
(even-better-transform -3 -3) ; 0.003
(even-better-transform 2 -2) ; -0.02