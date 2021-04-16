;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;PROBLEMA 3

(define interpretazione
  (lambda (x b)
    (let ((k (- (string-length x) 1)))
      (if (string=? x "")
          0
          (+ (* (expt b k) (val-cifra (string-ref x 0))) (interpretazione (substring x 1) b))
          )))) 

(define val-cifra
  (lambda (c)
    (- (char->integer c) (char->integer #\0))))

(define interpretazione2
  (lambda (x b)
    (let ((k (string-length x)))
      (if (string=? x "")
          0
          (+ (* (expt b (- 0 k)) (val-cifra (string-ref x (- k 1)))) (interpretazione2 (substring x 0 (- k 1)) b))
          )))) 

(define interpretazione+-
  (lambda (num b)
    (let ((q (string-ref num 0)))
      (cond ((char=? q #\+) (interpretazione (substring num 1) b))
            ((char=? q #\-) (- 0 (interpretazione (substring num 1) b)))
            ((or (char=? q #\0) (char=? q #\1)) (interpretazione num b))
            )
      ))) 
                            
(define count
  (lambda (num)
    (let ((a (string-ref num 0)))
      (if (> (string-length num) 1)
          (if (char=? a #\.)
              0
              (+ 1 (count (substring num 1))) )
              
          1)
      ))) 

(define bin-rep-number1
  (lambda (num b)
    (if (char=? (string-ref num 0) #\-)
        (if (< (count num) (string-length num))
            (- (interpretazione+- (substring num 0 (count num)) b) (interpretazione2 (substring num (+ (count num) 1)) b))
            (interpretazione+- num b))
        (if (< (count num) (string-length num))
            (+ (interpretazione+- (substring num 0 (count num)) b) (interpretazione2 (substring num (+ (count num) 1)) b))
            (interpretazione+- num b))
    )))

(define bin-rep-number
  (lambda (num)
    (bin-rep-number1 num 2)))
 
;parte 2
(define numval
  (lambda (base x k)
    (if (char=? (string-ref base 0) x)
        k
        (numval (substring base 1) x (+ k 1)) 
        )))

(define convert
  (lambda (base x)
    (if (string=? x "")
        0
        (+ (* (numval base (string-ref x 0) 0) (expt (string-length base) (- (string-length x) 1))) (convert base (substring x 1)))
        )))

(define convert2
  (lambda (base x)
    (if (string=? x "")
        0
        (+ (* (numval base (string-ref x (- (string-length x) 1)) 0) (expt (string-length base) (- 0 (string-length x)))) (convert2 base (substring x 0 (- (string-length x) 1))))
        )))

(define convert+-
  (lambda (base x)
    (let ((q (string-ref x 0)))
      (cond ((char=? q #\+) (convert base (substring x 1)))
            ((char=? q #\-) (- 0 (convert base (substring x 1))))
            (else (convert base x))
            )
      ))) 

(define rep-number
  (lambda (base x)
    (if (char=? (string-ref x 0) #\-)
        (if (< (count x) (string-length x))
            (- (convert+- base (substring x 0 (count x))) (convert2 base (substring x (+ (count x) 1))))
            (convert+- base x))
        (if (< (count x) (string-length x))
            (+ (convert+- base (substring x 0 (count x))) (convert2 base (substring x (+ (count x) 1))))
            (convert+- base x))
    ))) 


;perte1
(bin-rep-number "+1101"); 13
(bin-rep-number "0"); 0
(bin-rep-number "10110.011"); 22.375
(bin-rep-number "-0.1101001"); -0.8203125


;parte2
(rep-number "zu" "-uuzz"); -12
(rep-number "0123" "+21.1"); 9.25
(rep-number "01234" "-10.02"); -5.08
(rep-number "0123456789ABCDEF" "0.A"); 0.625













        
    
    
      