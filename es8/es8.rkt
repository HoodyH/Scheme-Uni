;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es8) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 8

(define prime-factors
  (lambda (x)
    (prime-factors1 x 2)
    )
  )
                    
(define prime-factors1
  (lambda (num a)
    (cond
      ((and (= (remainder num a)0) (not (= (quotient num a)0))) (append (list a) (prime-factors1 (quotient num a) a)))
      ((and (not (= (remainder num a)0)) (not (= (quotient num a)0))) (prime-factors1 num (+ a 1)))
      (else '())
      )
    )
  )



(define short-prime-factors
  (lambda (x)
    (short-prime-factors1 x 2 #false)
    )
  )

(define short-prime-factors1
  (lambda (num b d)
    (cond
      ((and (= (remainder num b)0) (not (= (quotient num b)0)) (not d)) (append (list b) (short-prime-factors1 (quotient num b) b #true)))
      ((and (= (remainder num b)0) (not (= (quotient num b)0)) d) (short-prime-factors1 (quotient num b) b #true))
      ((and (not (= (remainder num b)0)) (not (= (quotient num b)0))) (short-prime-factors1 num (+ b 1) #false))
      (else '())
      )
    )
  )


(define prime-facts-degs
  (lambda (x)
    (prime-facts-degs1 x 2 0)
    )
  )

(define prime-facts-degs1
  (lambda (num b count)
    (cond
      ((and (= (remainder num b)0) (not (= (quotient num b)0)) )  (prime-facts-degs1 (quotient num b) b (+ count 1)))
      ((and (not(= count 0)) (not (= (remainder num b)0)) (not (= (quotient num b)0))) (append (list (append(list b)(list count)))(prime-facts-degs1 num (+ b 1) 0)))
      ((and (not (= (remainder num b)0)) (not (= (quotient num b)0))) (prime-facts-degs1 num (+ b 1) 0))
      (else (append (list(append(list b)(list count)))))
      )
    )
  )



;parte 1
(prime-factors 7); '(7)
(prime-factors 9); '(3 3)
(prime-factors 28); '(2 2 7)
(prime-factors 39); '(3 13)
(prime-factors 540); '(2 2 3 3 3 5)
(prime-factors 1617); '(3 7 7 11)

;parte2
(short-prime-factors 7); '(7)
(short-prime-factors 9); '(3)
(short-prime-factors 28); '(2 7)
(short-prime-factors 39); '(3 13)
(short-prime-factors 540); '(2 3 5)
(short-prime-factors 1617); '(3 7 11)

;parte3
(prime-facts-degs 7); '((7 1))
(prime-facts-degs 9); '((3 2))
(prime-facts-degs 540); '((2 2) (3 3) (5 1))
(prime-facts-degs 1617); '((3 1) (7 2) (11 1))
(prime-facts-degs 39); '((3 1) (13 1))
(prime-facts-degs 28); '((2 2) (7 1))
