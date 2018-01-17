#lang eopl

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(define g
  (lambda (num-and-sexp lst)
    (if (null? lst)
        (list num-and-sexp)
        (cons num-and-sexp (cons (+ 1 (car num-and-sexp)) (cdr lst))))))