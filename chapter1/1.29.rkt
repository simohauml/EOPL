#lang eopl

; Listof(int) --> Listof(int)
; usage: (sort loi)

(define sort
  (lambda (loi)
    (cond
      ((null? loi) '())
      ((null? (cdr loi)) loi)
      ((< (car loi) (cadr loi)) (merge (list (car loi) (cadr loi)) (sort (cddr loi))))
      (else
       (merge (list (cadr loi) (car loi)) (sort (cddr loi)))))))

(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((< (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2)))
      (else
       (cons (car loi2) (merge loi1 (cdr loi2)))))))