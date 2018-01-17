#lang eopl

;Listof(sym) --> Listof(sym)
;usage: (product sos1 sos2)
;(define product-b
;  (lambda (sos1 sos2)
;    (if (null? sos1)
;        '()
;        (cons
;         (list
;          (car sos1)
;          sos2)
;         (product-b (cdr sos1) sos2)))))
;
;(define product
;  (lambda (sos1 sos2)
;    (if (null? sos2)
;        '()
;        (append
;         (product-b sos1 (car sos2))
;         (product sos1 (cdr sos2))))))

(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (map (lambda (s2) (list (car sos1) s2)) sos2)
                (product (cdr sos1) sos2)))))

;; test
(product '(a b c) '(x y))