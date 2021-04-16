;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es5) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 5

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


(manh 2 2 2)