;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es10) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 10

(define converti_lettera_orario       
  (lambda (string k i count kshift)
    
    (if (= i (string-length string))
        (converti_lettera_orario string k 0 count kshift)
        
        (let ([x (string-ref string i)]) 
          (cond
            ;scorro la stringa fino a che non trovo il carattere
            ((and (not(char=? x k)) (= count 0) )
               (converti_lettera_orario string k (+ i 1) 0 kshift))
            ;se ho trovato il carattere inizio lo shift
            ((and (char=? x k) (not(= kshift 0)) )
               (converti_lettera_orario string k (+ i 1) (+ count 1) kshift))
            ;se non ho finito di shiftare continuo
            ((and (not(= count kshift)) (not(= count 0)) )
               (converti_lettera_orario string k (+ i 1) (+ count 1) kshift))
            
            ((= count kshift) x)          
            )
          )
        )
    )
  )

(define converti_lettera_antiorario       
  (lambda (string k i count kshift)
    
    (if (= i -1)
        (converti_lettera_antiorario string k (-(string-length string)1) count kshift)
        
        (let ([x (string-ref string i)]) 
          (cond
            ;scorro la stringa fino a che non trovo il carattere
            ((and (not(char=? x k)) (= count 0) )
               (converti_lettera_antiorario string k (- i 1) 0 kshift))
            ;se ho trovato il carattere inizio lo shift
            ((and (char=? x k) (not(= kshift 0)) )
               (converti_lettera_antiorario string k (- i 1) (+ count 1) kshift))
            ;se non ho finito di shiftare continuo
            ((and (not(= count kshift)) (not(= count 0)) )
               (converti_lettera_antiorario string k (- i 1) (+ count 1) kshift))
            
            ((= count kshift) x)          
            )
          )
        )
    )
  )


(define cripta
  (lambda (frase kshift)
    (string-append
     
     (let ([k (string-ref frase 0)]) 
          (string(converti_lettera_antiorario "ABCDEFGHILMNOPQRSTVX" k (-(string-length "ABCDEFGHILMNOPQRSTVX")1) 0 kshift)))
     
       (if(=(string-length (substring frase 1))0) "" (cripta (substring frase 1) kshift)))
    )
  )



(define decripta
  (lambda (frase kshift)
    (string-append
     
     (let ([k (string-ref frase 0)]) 
          (string(converti_lettera_orario "ABCDEFGHILMNOPQRSTVX" k 0 0 kshift)))
     
       (if(=(string-length (substring frase 1))0) "" (decripta (substring frase 1) kshift)))
    )
  )
    
(cripta "PROVADIFVNXIONAMENTO" 3)
(decripta (cripta "PROVADIFVNXIONAMENTO" 3) 3)
(cripta "CIAO" 30)
(decripta (cripta "CIAO" 30) 30)

;Parte 1 v2

(define lst '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\V #\X))
(define valI (char->integer #\I))
(define valT (char->integer #\T))
(define valV (char->integer #\V))

(define normalize ;lista ordinata normalizzata dell'alfabeto (formato numerico)
  (map
   (lambda (c)
     (let ((i (char->integer c)))
       (cond ((> i valV) (- i 68))
             ((> i valT) (- i 67))
             ((> i valI) (- i 66))
             (else (- i 64))))) lst))

(define cp-cae ;rotazione dell'alfabeto (formato numerico)
  (lambda (r)
    (map (lambda (n)
           (if (> (+ n r) 20) (- (+ n r) 20)
               (if (< (+ n r) 1) (+ (+ n 20) r) (+ n r))))
         normalize)))

(define cae-cipher ;rotazione dell'alfabeto (formato letterale)
  (lambda (r)
    (map (lambda (n)
           (integer->char
            (cond ((> n 19) (+ n 68))
                  ((> n 18) (+ n 67))
                  ((> n 9) (+ n 66))
                  (else (+ n 64)))))
         (cp-cae r))))


;Parte 2

(define H
  (lambda (i s)
    (lambda (m n)
      (if (= n 0)
          (i m)
          (s m ((H i s) m (- n 1)))
          )
      )))

(define s2
  (lambda (m n)
    (+ n 1)
    ))

(define add (H (lambda (x) x) s2))
(define mul (H (lambda (x) 0) add))
(define pow (H (lambda (x) 1) mul))

(add 2 0)
(add 2 1)
(add 2 2)
(mul 4 6)
(pow 2 2)
(pow 2 8)