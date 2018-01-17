#lang eopl

; List --> Bool
; usage: (exists? pred lst)
(define exists?
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) #t)
      (else (exists? pred (cdr lst))))))

;; test
(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))