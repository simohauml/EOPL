#lang eopl

; List --> List
; usage: (flatten slist)
(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((list? (car slist)) (append (flatten (car slist)) (flatten (cdr slist))))
      (else (cons (car slist) (flatten (cdr slist)))))))

(define flatten-without-append
  (lambda (slist)
    (letrec ((flatten-onto
              (lambda (slist tail)
                (cond ((null? slist) tail)
                      ((symbol? (car slist))
                       (cons (car slist) (flatten-onto (cdr slist) tail)))
                      (else
                       (flatten-onto (car slist)
                                     (flatten-onto (cdr slist) tail)))))))
      (flatten-onto slist '()))))

;; test
(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))