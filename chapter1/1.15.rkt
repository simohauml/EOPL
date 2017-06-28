#lang eopl

;duple : Int * Listof(x) ---> Listof(x x ... )
;usage : (duple n x)
(define duple
  (lambda (n x)
    (if (eqv? n 0)
        '()
        (cons x
              (duple (- n 1) x)))))