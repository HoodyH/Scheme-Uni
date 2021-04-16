;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es6) (read-case-sensitive #t) (teachpacks ((lib "drawings.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.rkt" "installed-teachpacks")) #f)))
;PROBLEMA 6

;; L-Tessellation Problem
;; Claudio Mirolo, 6/11/2014

;; Per eseguire questo codice e' necessario
;; utilizzare il TeachPack "drawings.ss"

;;   (shift-down <figura> <passi>)
;;
;;   (shift-right <figura> <passi>)
;;
;;   (quarter-turn-right <figura>)
;;
;;   (quarter-turn-left <figura>)
;;
;;   (half-turn <figura>)
;;
;;   (glue-tiles <figura> <figura>)


(set-tessellation-shift-step!)


(define r1
  (lambda (x)
    (glue-tiles (fig x) (shift-right (quarter-turn-right (fig x)) (* x 2)))
  )
);end

(define r2
  (lambda (x)
    (glue-tiles (shift-down(shift-right (fig x) x) x) (shift-down (quarter-turn-left (fig x)) (* x 2)))
  )
);end


(define fig
  (lambda (x)
   (if (= x 1) L-tile (glue-tiles (r1 (*(/ x 4)2)) (r2 (*(/ x 4)2)) ) )
  )
);end (glue-tiles t1 t2)

(fig 2)
(fig 4)
(fig 8)
(fig 16)