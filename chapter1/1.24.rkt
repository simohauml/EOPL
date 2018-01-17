#lang eopl

; List --> Bool
; usage: (every? pred lst)

(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst)) (every? pred (cdr lst)))
      (else #f))))

;; test
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))