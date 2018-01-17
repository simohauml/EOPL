#lang eopl

;down : Listof(lst) ---> Listof(lst)
;usage : (down lst)
(define down
  (lambda (lst)
    (map list lst)))

;(define down
;  (lambda (lst)
;    (if (null? lst)
;        '()
;        (cons
;         (list (car lst))
;         (down (cdr lst))))))

;; test
(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))