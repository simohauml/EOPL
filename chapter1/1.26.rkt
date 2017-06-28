#lang eopl

; List --> List
; usage: (up lst)
(define up
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst)) (append (car lst) (up (cdr lst))))
      (else (cons (car lst) (up (cdr lst)))))))