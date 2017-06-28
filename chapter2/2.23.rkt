#lang eopl

;LcExp ::= Identifier
;::= (lambda (Identifier) LcExp)
;::= (LcExp LcExp)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define identifier?
  (lambda (x)
    (and (symbol? x) (not (eqv? 'lambda x)))))