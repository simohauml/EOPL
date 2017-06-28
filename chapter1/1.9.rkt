#lang eopl

;remove-first : Sym × Listof(Sym) → Listof(Sym)
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (cons (car los) (remove-first s (cdr los)))))))

;remove : Sym × Listof(Sym) → Listof(Sym)
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cons ' (remove s (cdr los)))
            (cons (car los) (remove s (cdr los)))))))