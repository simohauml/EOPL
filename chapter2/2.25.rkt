#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define-datatype sum-key sum-key?
  (sum-key-pair
   (sum integer?)
   (key symbol?)))

(define get-key
  (lambda (sk-pair)
    (cases sum-key sk-pair
      (sum-key-pair (sum key) key))))

(define get-sum
  (lambda (sk-pair)
    (cases sum-key sk-pair
      (sum-key-pair (sum key) sum))))

(define sum-interior
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) num)
      (interior-node (key left right)
                     (+ (sum-interior left)
                        (sum-interior right))))))

(define compair
  (lambda (sk-1 sk-2)
    (if (> (get-sum sk-1) (get-sum sk-2))
        sk-1
        sk-2)))

(define max-interior
  (lambda (bt)
    (get-key (max-interior-helper bt))))

(define max-interior-helper
  (lambda (bt)
    (cases bintree bt
      (leaf-node (num) (sum-key-pair num '_leaf))
      (interior-node (key left right)
                     (cases bintree left
                       (leaf-node (num-l)
                                  (cases bintree right
                                    (leaf-node (num-r)
                                               (sum-key-pair (+ num-l num-r) key))
                                    (interior-node (key-r left-r right-r)
                                                   (let* ((result-r (max-interior-helper right))
                                                          (result-cur (sum-key-pair (sum-interior bt) key)))
                                                     (compair result-cur result-r)))))
                       (interior-node (key-l left-l right-l)
                                      (cases bintree right
                                        (leaf-node (num-r)
                                                   (let* ((result-l (max-interior-helper left))
                                                          (result-cur (sum-key-pair (sum-interior bt) key)))
                                                     (compair result-cur result-l)))
                                        (interior-node (key-r left-r right-r)
                                                       (let* ((result-l (max-interior-helper left))
                                                              (result-r (max-interior-helper right)))
                                                         (compair result-l result-r))))))))))
                                                   



                                                    
(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))