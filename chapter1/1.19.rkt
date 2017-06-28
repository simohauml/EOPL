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