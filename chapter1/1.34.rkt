#lang eopl

(define path
  (lambda (n bst)
    (cond
      ((null? bst) "Not found")
      ((eqv? n (car bst)) '())
      ((< n (car bst)) (cons 'left (path n (cadr bst))))
      (else
       (cons 'right (path n (caddr bst)))))))

;; test
(equal? (path 17 '(14 (7 () (12 () ()))
                       (26 (20 (17 () ()) ())
                           (31 () ()))))
         '(right left left))