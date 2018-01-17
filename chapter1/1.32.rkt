#lang eopl

(define leaf (lambda (i) i))
(define interior-node (lambda (s i1 i2) (list s i1 i2)))
(define leaf? integer?)
(define lson cadr)
(define rson caddr)
(define contents-of (lambda (b) (if (leaf? b) b (car b))))


(define double-tree
  (lambda (t)
    (if (leaf? t)
        (leaf (* 2 (contents-of t)))
        (interior-node (contents-of t) (double-tree (lson t)) (double-tree (rson t))))))

;; test
(define t1 (interior-node 'a (leaf 1) (leaf 5)))
(define t2 (interior-node 'b (leaf 7) (leaf 9)))
(define t3 (interior-node 'c t1 t2))

(double-tree t3)