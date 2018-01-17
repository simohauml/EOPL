#lang racket

;;invert : Listof(List) ---> Listof(List)
;;usage : (invert lst)
(define invert
  (lambda (lst)
    (map (lambda (2-lst) (list (cadr 2-lst) (car 2-lst)))
         lst)))

;#lang eopl
;
;;invert : Listof(List) ---> Listof(List)
;;usage : (invert lst)
;(define invert
;  (lambda (lst)
;    (if (null? lst)
;        '()
;        (cons
;         (cons (cadar lst) (caar lst))
;         (invert (cdr lst))))))

;; test
(invert '((a 1) (a 2) (1 b) (2 b)))