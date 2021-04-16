;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es1.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;PROBLEMA 1

(define soggetto
  (lambda (sogg)
    (if  (or (char=? (string-ref sogg(-(string-length sogg) 1 )) #\o ) (char=? (string-ref sogg(-(string-length sogg) 1 )) #\a ) ) "singolare" "plurale"  )

  ) ;end lambda
 ) ;end function soggetto

(define soggetto1
  (lambda (sogg)
    (if (string=? (soggetto sogg) "singolare")
      (if (char=? (string-ref sogg(-(string-length sogg) 1 )) #\o ) (string-append "il " sogg) (string-append "la " sogg))
      (if (char=? (string-ref sogg(-(string-length sogg) 1 )) #\i ) (string-append "i " sogg) (string-append "le " sogg))
     );end if soggetto

   ) ;end lambda
 ) ;end function soggetto1

(define verbo
  (lambda (sogg verb)
    
     (if (string=? (soggetto sogg) "singolare")
         (cond
           ((string=? (substring verb (-(string-length verb) 3)) "are") (string-append (substring verb 0 (-(string-length verb)3)) "a ") )
           ((or(string=? (substring verb (-(string-length verb) 3)) "ere")(string=? (substring verb (-(string-length verb) 3)) "ire"))
             (string-append (substring verb 0 (-(string-length verb)3)) "e ") )
           );end cond1
         (cond
           ((string=? (substring verb (-(string-length verb) 3)) "are") (string-append (substring verb 0 (-(string-length verb)3)) "ano ") )
           ((or(string=? (substring verb (-(string-length verb) 3)) "ere")(string=? (substring verb (-(string-length verb) 3)) "ire"))
             (string-append (substring verb 0 (-(string-length verb)3)) "ono ") )
           );end cond2
      );end if soggetto
    
  ) ;end lambda
) ;end function soggetto1

(define frase
  (lambda (sogg verb ogg)
    (string-append (string-append (string-append (soggetto1 sogg) " ") (verbo sogg verb)) (soggetto1 ogg))
    ) ;end lambda
) ;end function soggetto1


(frase "gatto" "cacciare" "topi")
(frase "mucca" "mangiare" "fieno")
(frase "sorelle" "leggere" "novella")
(frase "bambini" "amare" "favole")
(frase "musicisti" "suonare" "pianoforti")