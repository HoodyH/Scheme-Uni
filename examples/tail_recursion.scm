
;; Questo file contiene esempi di codice Scheme:
;; Ricorsione di coda e correttezza - 5/11/03
;; Ultimo aggiornamento: 3/12/15


;; Massimo Comun Divisore (MCD)

(define gcd            ; valore: naturale
  (lambda (x y)        ; x, y: naturali positivi
    (cond ((= x y) x)
          ((< x y) (gcd x (- y x)))
          (else    (gcd (- x y) y))  ; x > y
          )
    ))


;; Algoritmo del contadino Russo per la moltiplicazione:
;; Procedura che applica la ricorsione di coda

(define peasant-muliplication  ; valore: naturale
  (lambda (m n)                ; m, n: naturali
    (peasant-mul-tr m n 0)
    ))                         ; valore risultante: mn

(define peasant-mul-tr         ; valore: naturale
  (lambda (x y z)              ; x, y, z: naturali
    (cond ((= y 0)
           z)
          ((even? y)
           (peasant-mul-tr (* 2 x) (quotient y 2) z))
          (else                ; y dispari
           (peasant-mul-tr (* 2 x) (quotient y 2) (+ z x)))
          )
    ))                         ; valore risultante: z + xy

;; Dimostra per induzione che:
;;
;;   (peasant-mul-tr x y z)  -*->  z + xy
;;
;; Quindi dimostra che:
;;
;;   (peasant-muliplication m n)  -*->  mn


;; Quali funzioni di n calcolano
;; le procedure odd e unknown?

(define odd      ; valore: ?
  (lambda (i)    ; i > 0 naturale
    (if (= i 1)
        1
        (+ (odd (- i 1)) 2)
        )
    ))

(define unknown  ; valore: ?
  (lambda (x)    ; x: naturale
    (if (= x 0)
        0
        (+ (unknown (- x 1)) (odd x))
        )
    ))

;; Una volta individuate le formule che rappresentano il valore
;; delle espressioni (odd n) e (unknown n) al variare di n nel
;; rispettivo dominio (specifiche), dimostrane la correttezza.


;; Qual'e' la sequenza dei valori di (ufo n) per n = 1, 2, 3, ...?
;; Che proprieta' ha?

(define ufo            ; valore: ?
  (lambda (x)          ; x > 0 naturale
    (cond ((= x 1) 1)
          ((even? x)   ; x pari
           (- (* 2 (ufo (quotient x 2))) 1))
          (else        ; x dispari
           (+ (* 2 (ufo (quotient x 2))) 1))
          )
    ))

;; Identifica le proprieta' del valore dell'espressione (ufo n)
;; in relazione al valore n del parametro.


;; Quale funzioni di n calcola la procedura mistery?
;; (integrazione del 29/10/04 --- ricorsione di coda)

(define mistery        ; valore: ?
  (lambda (n)          ; n naturale
    (process  n  1 0)
    ))

(define process        ; valore: ?
  (lambda (i  x y)     ; i, x, y naturali, x > 0
    (if (= i 0)
        y
        (process  (- i 1)  (+ x 2) (+ y x))
        )
    ))

;; Una volta individuata la formula che rappresenta il valore
;; dell'espressione (mistery n) al variare di n nei naturali
;; (specifiche), dimostra la correttezza di "mistery"
;; riconducendola alla correttezza di "process".

