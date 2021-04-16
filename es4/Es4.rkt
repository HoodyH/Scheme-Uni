;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Es4) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 4

(define lsd
  (lambda (str1)
    (let ([x (string-length str1)])
      (if (= x 0) #\. (string-ref str1 (- x 1) ) )
    )
  )
);end


(define normalized-btr
  (lambda (str1)
    (let ([q (string-ref str1 0)])
      (if
       (char=? q #\.)
          (if (= (string-length str1)1) "." (normalized-btr (substring str1 1 (string-length str1)))  )
        str1 )
    )
  )
);end


(define head
  (lambda (str1)
    (if (= (string-length str1) 0) "" (substring str1 0 (-(string-length str1)1) ) )
   )
);end


(define btr-digit-sum                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond
      ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\.)
                        ((char=? c #\.)  ; - - .
                         #\+)
                        ((char=? c #\+)  ; - - +
                         #\-)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\+)
                        ((char=? c #\.)  ; - . .
                         #\-)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+) c)))    ; - + c
                  
      ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\+)
                        ((char=? c #\.)  ; . - .
                         #\-)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.) c)      ; . . c
                  
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\+)
                        ((char=? c #\+)  ; . + +
                         #\-)))))
      
      ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\+)
                        ((char=? c #\+)  ; + . +
                         #\-)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\+)
                        ((char=? c #\.)  ; + + .
                         #\-)
                        ((char=? c #\+)  ; + + +
                         #\.)))))
          )
    )
  )


(define btr-carry                 
  (lambda (c1 c2)
    (cond
          ((char=? c1 #\-)               
           (cond
             ((char=? c2 #\-) #\-) ;-- > -
             (else #\.)
           ));end cond
          ((char=? c1 #\+)               
           (cond
             ((char=? c2 #\+) #\+) ;++ > +
             (else #\.)
           ));end cond
          (else #\.)
          )
    )
  );end




(define btr-carry-sum
  (lambda (str1 str2 r)

       (string-append
        
        (string(btr-digit-sum (lsd  str1) (lsd  str2) r))
        
        (if
         (and (= 0 (string-length str1)) (= 0 (string-length str1)) )
          ""
         (btr-carry-sum (head  str1) (head  str2) (btr-carry (lsd str1) (lsd str2)))
         )
        
        )
    )
);end


(define btr-sum
  (lambda (str1 str2)


     (normalized-btr
      (list->string (reverse (string->list (btr-carry-sum (normalized-btr str1) (normalized-btr str2)  #\.))))
     )
  )
);end




(btr-sum "-+--" "+");"-+-."
(btr-sum "+-.+" "-+.-");"."
(btr-sum "-+-+." "-.-+");"-.-.+" 
(btr-sum "+-+-." "+.+-");"+.+.-"

