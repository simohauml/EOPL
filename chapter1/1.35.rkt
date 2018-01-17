#lang eopl

(define leaf (lambda (i) i))
(define interior-node (lambda (s i1 i2) (list s i1 i2)))
(define leaf? integer?)
(define lson cadr)
(define rson caddr)
(define contents-of (lambda (b) (if (leaf? b) b (car b))))

;(define number-leaves
;  (lambda (b)
;    (let ([x 0])
;      (letrec
;          ((loop
;            (lambda (bb)
;              (if (leaf? bb)
;                  (leaf x)
;                  (interior-node
;                   (contents-of bb)
;                   (loop (lson b))
;                   (loop (rson b)))))))
;      (loop b)))))
;
;(define number-leaves-from
;  (lambda (n b)
;    (if (leaf? b)
;        (leaf n)
;        (interior-node
;         (contents-of b)
;         (number-leaves-from n (lson b))
;         (number-leaves-from (+ 1 n) (rson b))))))

; let-values and friends are not part of #eopl, so we simply use a pair.
(define number-leaves
  (lambda (b)
    (letrec ((loop
              (lambda (b n)
                (if (leaf? b)
                    (cons n (+ n 1))
                    (let* ((bnl (loop (lson b) n))
                           (bnr (loop (rson b) (cdr bnl))))
                      (cons (interior-node (contents-of b) (car bnl) (car bnr))
                            (cdr bnr)))))))
      (car (loop b 0)))))

;; test
(equal? (number-leaves
         (interior-node 'foo
                        (interior-node 'bar
                                       (leaf 26)
                                       (leaf 12))
                        (interior-node 'baz
                                       (leaf 11)
                                       (interior-node 'quux
                                                      (leaf 117)
                                                      (leaf 14)))))
        '(foo
          (bar 0 1)
          (baz 2 (quux 3 4))))