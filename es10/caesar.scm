;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname caesar) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks") (lib "hanoi.rkt" "installed-teachpacks")) #f)))

;; Esempio: Codice di Giulio Cesare
;;
;; Temi affrontati:
;; 1. Procedure con argomenti procedurali
;; 2. Procedure con valori procedurali
;; 3. Procedure con argomenti e valori procedurali
;; Ultimo aggiornamento: 21/12/15


;; 1. Procedure con argomenti procedurali
;;
;; Semplice tecnica di crittazione di un messaggio:
;; ad ogni lettera viene applicata la stessa regola
;; di codifica indipendentemente dal contesto

(define encryption        ; valore: stringa
  (lambda (message rule)  ; message: stringa, rule: lettera -> lettera
    (if (= (string-length message) 0)
        ""
        (string-append
         (string (rule (string-ref message 0)))
         (encryption (substring message 1) rule)
         ))
    ))

;; Qual e' il risultato dell'applicazione:
;;
;;   (encryption "PROGRAMMAZIONE" (lambda (x) x)) ?


;; 2. Procedure con valori procedurali
;;
;; Regole che applicano lo schema del codice di Giulio Cesare:
;; Rotazioni di lettere

(define caesar-cipher     ; valore procedurale: lettera -> lettera
  (lambda (rot)           ; la funzione dipende da rot: integer
    (lambda (letter)      ; espressione procedurale: lettera -> lettera
      (let ((c (+ (char->integer letter) rot)))
        (if (> c codZ)
          (integer->char (- c 26))  ; 26 lettere dell'alfabeto Inglese
          (integer->char c)
          )))
    ))

(define codA (char->integer #\A))  ; codice ASCII della lettera A (costante)
(define codZ (char->integer #\Z))  ; codice ASCII della lettera Z (costante)


;; Esercizio:
;; Che valore assume la seguente espressione?
;;
;;   (encryption (encryption "PROGRAMMAZIONE" (caesar-cipher 3)) (caesar-cipher 23))
;;
;; Perche'?
;;
;; Esercizio:
;; Revisione della procedura "caesar-cipher" applicabile
;; esattamente all'alfabeto Latino (non a quello Inglese).
;; Le lettere dell'alfabeto al tempo di Cesare sono 20:
;;
;;   A B C D E F G H I L M N O P Q R S T V X


;; 3. Procedure con argomenti e valori procedurali
;;
;; Funzione di decrittazione data la funzione di crittazione
;; (biiezione che realizza una permutazione delle lettere maiuscole dell'alfabeto inglese):
;; "Rottura del codice"

(define compl-rotation  ; valore procedurale: lettera -> lettera
  (lambda (rule)        ; argomento procedurale rule: lettera -> lettera
    (caesar-cipher
      (- 26 (- (char->integer (rule #\A)) codA))  ; rotazione complementare
      )
    ))


;; Esempio:
;;
;;   (define enc-rule (caesar-cipher 3))
;;
;;   enc-rule
;;
;;   (encryption "IULIUSCAESAR" enc-rule)
;;
;;   (encryption "LXOLXVFDHVDU" (compl-rotation enc-rule))


;; Piu' in generale:

(define compl-permutation  ; valore procedurale: lettera -> lettera
  (lambda (rule)           ; argomento procedurale rule: lettera -> lettera
    (lambda (letter) (find-from codA letter rule))
    ))

(define find-from               ; valore: lettera
  (lambda (sample letter rule)  ; rule: lettera -> lettera, sample: intero, letter: lettera
    (let ((c (integer->char sample)))
      (if (char=? (rule c) letter)
        c
        (find-from (+ sample 1) letter rule)
        ))
    ))


;; Esempio di applicazione:
;;
;; (define enc-rule (caesar-cipher 3))
;; (define dec-rule (compl-permutation enc-rule))
;; (define encrypted-message (encryption "JULIUSCAESAR" enc-rule))
;; encrypted-message
;; (encryption encrypted-message dec-rule)


;; Crittazione di sequenze (liste) di parole:

(define words-encryption  ; lista di stringhe
  (lambda (enc words)     ; enc: procedura, words: lista di stringhe
    (if (null? words)
        null
        (cons (enc (car words)) (words-encryption enc (cdr words)))
        )))


;; Esempio:
;;
;;   (define caesar-rule (caesar-cipher 3))
;;   (words-encryption (lambda (w) (encryption w caesar-rule)) '("ALEA" "IACTA" "EST"))
;;   (words-encryption (lambda (w) (encryption w (compl-permutation caesar-rule))) '("DOHD" "LDFWD" "HVW"))
;;
;; Ma anche:
;;
;;   (map (lambda (w) (encryption w caesar-rule)) '("ALEA" "IACTA" "EST"))
;;   (map (lambda (w) (encryption w (compl-permutation caesar-rule))) '("DOHD" "LDFWD" "HVW"))


;; Operatori di ordine superiore: composizione funzionale e iterazione

;; Composizione funzionale

(define composition     ;  composizione di funzioni g, f : D -> D
  (lambda (g f)         ;  rappresentate da procedure in Scheme
    (lambda (x) (g (f x)))
    ))


;; Composizione iterata di una funzione

(define iterate         ;  valore procedurale: D -> D
  (lambda (f i)         ;  f : D -> D funzione, i naturale
    (if (= i 0)
        (lambda (x) x)  ; funzione identica, per i=0
        (composition f (iterate f (- i 1)))
        )
    ))

;; Ma anche:
;;
;; (define iterate       ;  valore procedurale: D -> D
;;   (lambda (f i)       ;  f : D -> D funzione, i naturale
;;     (lambda (x)
;;       (if (= i 0)
;;           x
;;           (f ((iterate f (- i 1)) x))
;;           ))
;;     ))


;; Esercizio:
;;
;;   (define some-rule (composition (iterate (caesar-cipher 1) 7) (caesar-cipher 19)))
;;
;; Che regola di codifica realizza "some-rule"?

