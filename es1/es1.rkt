;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es1lab) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(define soggetto
  (lambda (sogg)
    (if (or (char=? (string-ref sogg (-(string-length sogg) 1)) #\o) (char=? (string-ref sogg (-(string-length sogg) 1)) #\a))   
        "singolare"
        "plurale")))

(define soggetto1
  (lambda (sogg)
    (if (string=? (soggetto sogg) "singolare")
        (if (char=? (string-ref sogg (-(string-length sogg) 1)) #\o)
            (string-append "il" (string-append " " sogg))
            (string-append "la" (string-append " " sogg)))
        (if (char=? (string-ref sogg (-(string-length sogg) 1)) #\i)
            (string-append "i" (string-append " " sogg))
            (string-append "le" (string-append " " sogg)))
        )
    ))
            
        

(define verbo 
  (lambda (predverb sogg)
    (if (string=? (soggetto sogg)  "singolare")
         (cond ((string=? (substring predverb (-(string-length predverb ) 3)) "are") (substring predverb 0 (-(string-length predverb) 2)))
               ((string=? (substring predverb (-(string-length predverb ) 3)) "ere") (substring predverb 0 (-(string-length predverb) 2)))
               ((string=? (substring predverb (-(string-length predverb ) 3)) "ire") (string-append (substring predverb 0 (-(string-length predverb) 3)) "e")))
         (cond ((string=? (substring predverb (-(string-length predverb ) 3)) "are") (string-append (substring predverb 0 (-(string-length predverb) 2)) "no"))
               ((string=? (substring predverb (-(string-length predverb ) 3)) "ere") (string-append (substring predverb 0 (-(string-length predverb) 3)) "ono"))
               ((string=? (substring predverb (-(string-length predverb ) 3)) "ire") (string-append (substring predverb 0 (-(string-length predverb) 3)) "ono")))
        )))

(define oggetto
  (lambda (ogg)
    (cond ((char=? (string-ref ogg (-(string-length ogg) 1)) #\o) (string-append "il" (string-append " " ogg)))
          ((char=? (string-ref ogg (-(string-length ogg) 1)) #\a) (string-append "la" (string-append " " ogg)))
          ((char=? (string-ref ogg (-(string-length ogg) 1)) #\i) (string-append "i" (string-append " " ogg)))
          ((char=? (string-ref ogg (-(string-length ogg) 1)) #\e) (string-append "le" (string-append " " ogg)))
          )
    ))

(define frase 
  (lambda (sogg predverb ogg)
    (string-append (string-append (string-append (string-append (soggetto1 sogg) " ") (verbo predverb sogg)) " ") (oggetto ogg))))     
    
              
             

    