#lang eopl

; List ---> List
; usage: (list-set lst n x)
(define list-set
  (lambda (lst n x)
    (cond ((null? lst) (report-list-too-short n))
          ((zero? n) (cons x (cdr lst)))
          (else (cons (car lst)
                      (list-set (cdr lst) (- n 1) x))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'list-set
                "List too short by ~s elements.~%" (+ n 1))))

;; test
(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)