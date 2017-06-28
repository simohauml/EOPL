#lang eopl

;down : Listof(lst) ---> Listof(lst)
;usage : (down lst)

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (list (car lst))
         (down (cdr lst))))))