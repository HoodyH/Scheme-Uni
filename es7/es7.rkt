;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es7) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 7

;il punto nella piramide identificata da 3 variabili


;dati del problema:
;
;numero di livelli della struttura
;coordinate del punto da raggiundere

;metodo 1
(define prob-3D_met1
  (lambda (n i j)  ;n livelli, i rossi, j verdi
        (cond
          ((and (> n (+ i j)) (not (= i 0)) (not (= j 0))) (+ (* 1/3 (prob-3D_met1 (- n 1) i j)) (* 1/3 (prob-3D_met1 (- n 1) (- i 1) j)) (* 1/3 (prob-3D_met1 (- n 1) i (- j 1))) ))
          ((and (> n (+ i j)) (not (= i 0))) (+ (* 1/3 (prob-3D_met1 (- n 1) i j)) (* 1/3 (prob-3D_met1 (- n 1) (- i 1) j))))
          ((and (> n (+ i j)) (not (= j 0))) (+ (* 1/3 (prob-3D_met1 (- n 1) i j)) (* 1/3 (prob-3D_met1 (- n 1) i (- j 1)))))
          ((> n (+ i j)) (* 1/3(prob-3D_met1 (- n 1) i j)))
          ((and (not (= i 0)) (not (= j 0))) (+ (* 1/3 (prob-3D_met1 (- n 1) i (- j 1))) (* 1/3 (prob-3D_met1 (- n 1) (- i 1) j))))
          ((not (= i 0)) (* 1/3 (prob-3D_met1 (- n 1) (- i 1) j)))
          ((not (= j 0)) (* 1/3 (prob-3D_met1 (- n 1) i (- j 1))))
          (else 1)
        )
  )
)

;metodo 2
(define manh
  (lambda (i j k)

    (cond
      ((and (= i 0)(= j 0)) 1)
      ((and (= i 0)(= k 0)) 1)
      ((and (= k 0)(= j 0)) 1)
      ((= i 0) (+ (manh 0 (- j 1) k) (manh 0 j (- k 1))) )
      ((= j 0) (+ (manh (- i 1) 0 k) (manh i 0 (- k 1))) )
      ((= k 0) (+ (manh (- i 1) j 0) (manh i (- j 1) 0)) ) 
      

      (else (+ (manh(- i 1) j k) (manh i (- j 1) k) (manh i j (- k 1))))

     );and cond   
   );end lambda
);end

(define prob-3D_met2
  (lambda (n i j)
    (cond ((> (+ i j) n) "impossibile") ;se n è minore di i+j è impossibile
          (else (cond ((= n 0) 1)
                      (else (/ (manh i j (- n (+ i j))) (expt 3 n))) ; siccome la somma dei tratti blu verdi e rossi deve essere = n
                      ;allora date 2 coordinate, si trova la terza sottraendo a n i+j --> (- n (+ i j)
                 )
          )
      )
    )
  )



    

(prob-3D_met1 1 0 1); 1/3
(prob-3D_met1 3 1 1); 2/9
(prob-3D_met1 6 2 2); 10/81
(prob-3D_met1 2 1 0); 2/9
(prob-3D_met1 4 1 2); 4/27
;(prob-3D_met1 15 5 5); 28028/531441

(prob-3D_met2 1 0 1); 1/3
(prob-3D_met2 3 1 1); 2/9
(prob-3D_met2 6 2 2); 10/81
(prob-3D_met2 2 1 0); 2/9
(prob-3D_met2 4 1 2); 4/27
;(rob-3D_met2 15 5 5); 28028/531441