#lang eopl

;invert : Listof(List) ---> Listof(List)
;usage : (invert lst)
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (cons (cadar lst) (caar lst))
         (invert (cdr lst))))))