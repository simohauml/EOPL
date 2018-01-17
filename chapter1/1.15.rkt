#lang racket

;;duple : Int * Listof(x) ---> Listof(x x ... )
;;usage : (duple n x)
(define duple
  (lambda (n x)
    (build-list n (lambda (y) x))))

;#lang eopl
;
;;duple : Int * Listof(x) ---> Listof(x x ... )
;;usage : (duple n x)
;(define duple
;  (lambda (n x)
;    (if (eqv? n 0)
;        '()
;        (cons x
;              (duple (- n 1) x)))))

;; test
(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))
