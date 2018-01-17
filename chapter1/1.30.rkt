#lang eopl

(define sort/predicate
  (lambda (pred loi)
    (cond
      ((null? loi) '())
      ((null? (cdr loi)) loi)
      ((pred (car loi) (cadr loi)) (merge/predicate pred (list (car loi) (cadr loi)) (sort/predicate pred (cddr loi))))
      (else
       (merge/predicate pred (list (cadr loi) (car loi)) (sort/predicate pred (cddr loi)))))))

(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) loi1)
          ((pred (car loi1) (car loi2))
           (cons (car loi1) (merge/predicate pred (cdr loi1) loi2)))
          (else
           (cons (car loi2) (merge/predicate pred loi1 (cdr loi2)))))))

;; test
(sort/predicate < '(8 2 5 2 3))
(sort/predicate > '(8 2 5 2 3))