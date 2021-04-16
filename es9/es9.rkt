;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es9) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 9

;Restituisce la lista di mosse necessarie per completare la torre
(define hanoi-moves      ; val: lista di coppie
  (lambda (n)            ; n > 0 intero
    (hanoi-rec n 1 2 3)
    )
  )

(define hanoi-rec    ; val: lista di coppie
  (lambda (n s t d)  ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          )
        )
    )
  )

;SVOLGIMENTO PROBLMEMA PARTE 1
;restituisce la lista di mosse necessaria per raggiungere la mossa indicata
(define hanoi-disks
  (lambda (n k)
    (hanoi-disks1 n k 1 2 3 0 0 0)
    )
  )

(define hanoi-disks1
  (lambda (n k a b c s t d)
    (if (= n 0)
        (list (list a s) (list b t) (list c d))
        (if (>= k (expt 2 (- n 1)))
            (hanoi-disks1 (- n 1) (- k (expt 2 (- n 1))) c b a d (+ t 1) s)
            (hanoi-disks1 (- n 1) k a c b (+ s 1) d t))
        )
    )
  )

;SVOLGIMENTO PROBLMEMA PARTE GRAFICA
(define hanoi-picture
  (lambda (n k)
    (hanoi-picture1 n k 1 2 3 0 0 0 n (towers-background n))
    )
  )

(define hanoi-picture1
  (lambda (n k a b c s t d nc img)
    (if (= n 0)
        img
        (if (>= k (expt 2 (- n 1)))
            (hanoi-picture1 (- n 1) (- k (expt 2 (- n 1))) c b a d (+ t 1) s nc (above (disk-image n nc b t) img))
            (hanoi-picture1 (- n 1) k a b c (+ s 1) d t nc (above (disk-image n nc a s) img))
            )
        )
    )
  )

(hanoi-moves 3)
(hanoi-disks 4 8)
(hanoi-picture 4 8)