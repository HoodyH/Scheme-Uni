
;; Questo file contiene esempi di codice Scheme:
;; Ricorsione generale "ad albero"
;; Ultimo aggiornamento: 27/11/15


;; Numeri di Fibonacci (tree recursion)

(define fibonacci  ; valore: naturale
  (lambda (n)      ; n: naturale
    (if (< n 2)
        1
        (+ (fibonacci (- n 2)) (fibonacci (- n 1)))
        )
    ))

;; Come si puo' dimostrare che  (3/2)^(n-1) <= Fib(n) <= 2^(n-1) ?


;; Modello di crescita di una popolazione di conigli discusso
;; nel "Liber Abbaci" di Leonardo Pisano (1202).
;;
;; - All'istane iniziale t=0 c'e' una coppia di conigli fertili;
;; - I conigli nati all'istante t diventano fertili esattamente dopo
;;   un mese, all'istante t+1;
;; - Una coppia di conigli fertile all'istante t da' alla luce una
;;   nuova coppia di conigli ad ogni mese successivo t+1, t+2, ...
;; - I conigli non muoiono nell'intervallo di tempo considerato;
;; - I conigli nascono sempre a coppie: un maschio e una femmina.

(define fertile-pairs       ; valore: naturali
  (lambda (n)               ; n: naturali
    (if (= n 0)
        1
        (+ (newly-born-pairs (- n 1)) (fertile-pairs (- n 1)))
        )
    ))

(define newly-born-pairs    ; valore: naturali
  (lambda (n)               ; n: naturali
    (if (= n 0)
        0
        (fertile-pairs (- n 1))
        )
    ))

;; Quante coppie di conigli ci saranno dopo un anno?


;; Problema dei "Percorsi di Manhattan"

(define manhattan       ; valore: naturale
  (lambda (down right)  ; down, right: naturali
    (if (or (= down 0) (= right 0))
        1
        (+ (manhattan (- down 1) right)
           (manhattan down (- right 1)))
        )
    ))


;; Qualche giochetto con i numeri...

(define diagonal-sum
  (lambda (s i j)
    (if (< i 0)
        0
        (+ (manhattan i j) (diagonal-sum s (- i s) (+ j 1)))
        )
    ))

;; Che sequenza di numeri risulta dalle valutazioni
;; di  (diagonal-sum 1 n 0)  al variare di n?
;;
;; E dalle valutazioni
;; di  (diagonal-sum 2 n 0)  al variare di n?


;; Numeri di Stirling del II tipo:
;; In quanti modi diversi posso partizionare un insieme di n elementi
;; in k sottoinsiemi non vuoti e disgiunti?

(define stirling  ; valore: naturale
  (lambda (n k)   ; n, k naturali, k in [1,n]
    (if (or (= k 1) (= k n))
        1
        (+ (stirling (- n 1) (- k 1))
           (* k (stirling (- n 1) k)))
        )
    ))


;; Lunghezza della sottosequenza comune piu' lunga
;; Length of the Longest Common Subsequence (LLCS)

(define llcs     ; valore: intero
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           0)
          ((char=? (string-ref u 0) (string-ref v 0))
           (+ 1 (llcs (substring u 1) (substring v 1))))
          (else
           (max (llcs (substring u 1) v) (llcs u (substring v 1))))
          )))


;; Sottosequenza comune piu' lunga -- campione
;; Longest Common Subsequence (LCS)

(define lcs      ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           "")
          ((char=? (string-ref u 0) (string-ref v 0))
           (string-append (substring u 0 1)
                          (lcs (substring u 1) (substring v 1))))
          (else
           (longer (lcs (substring u 1) v) (lcs u (substring v 1))))
          )))

(define longer  ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (if (< (string-length u) (string-length v))
        v
        u
        )))

;; Cosa accade con questa diversa definizione di "longer"?
;;
;;   (define longer  ; valore: stringa
;;     (lambda (u v)  ; u, v: stringhe
;;       (cond ((> (string-length u) (string-length v))
;;              u)
;;             ((< (string-length u) (string-length v))
;;              v)
;;             ((= (random 2) 0)
;;              u)
;;             (else
;;              v)
;;             )))


;; Elenco delle sottosequenze comuni piu' lunghe -- tutte
;; Longest Common Subsequences (LCSs)

(define lcss     ; valore: stringa
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           (list ""))
          ((char=? (string-ref u 0) (string-ref v 0))
           (prefix-all (substring u 0 1)
                       (lcss (substring u 1) (substring v 1))))
          (else
           (let ((m (llcs (substring u 1) v))
                 (n (llcs u (substring v 1)))
                 )
             (cond ((< m n)
                    (lcss u (substring v 1)))
                   ((> m n)
                    (lcss (substring u 1) v))
                   (else
                    (merge (lcss (substring u 1) v) (lcss u (substring v 1))))
                   )))
          )))


;; Procedure di supporto

(define prefix-all     ; val: lista di stringhe
  (lambda (pre words)  ; pre: stringa, words: lista di stringhe
    (if (null? words)
        null
        (cons (string-append pre (car words))
              (prefix-all pre (cdr words)))
        )))

(define merge          ; val: lista di stringhe
  (lambda (ws1 ws2)    ; ws1, ws21: liste di stringhe
    (if (null? ws1)
        ws2
        (merge (cdr ws1)
               (ins (car ws1) ws2))
        )))

(define ins            ; val: lista di stringhe
  (lambda (w words)    ; w: stringa, words: lista di stringhe
    (cond ((null? words)
           (list w))
          ((string=? w (car words))
           words)
          (else
           (cons (car words) (ins w (cdr words))))
          )))

