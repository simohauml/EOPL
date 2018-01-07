#lang eopl

; ------------------------------------------------------------------------------
; Exercise 1.1

; ------------------------------------------------------------------------------
; Exercise 1.1.1
;
; {3n+2 | n in N} = {2, 5, 8, ...}
;
; top-down: A natural number n is in S if and only if
;    1) n = 2, or
;    2) n-3 in S.
;
; bottom-up: Define the set S to be the smallest set contained in N and
; satisfying the following 2 conditions:
;    1) 2 in S, and
;    2) if n in S, then n+3 in S.
;
; rules of inference:
;               n in S
;    ------   ----------
;    2 in S   (n+3) in S

; ------------------------------------------------------------------------------
; Exercise 1.1.2
;
; {2n+3m+1 | n,m in N} = {1, 3, 5, 7, ...
;                         4, 6, 8, 10, ...
;                         7, 9, 11, 13, ...}
;
; top-down: A natural number n is in S if and only if
;    1) n = 1, or
;    2) n-2 in S, or
;    3) n-3 in S.
;
; bottom-up: Define the set S to be the smallest set contained in N and
; satisfying the following 3 conditions:
;    1) 1 in S, and
;    2) if n in S, then n+2 in S, and
;    3) if n in S, then n+3 in S.
;
; rules of inference:
;               n in S       n in S
;    ------   ----------   ----------
;    1 in S   (n+2) in S   (n+3) in S

; ------------------------------------------------------------------------------
; Exercise 1.1.3
;
; {(n,2n+1) | n in N} = {(0,1), (1,3), (2,5), ...}
;
; top-down: A pair of natural numbers (n,m) is in S if and only if
;    1) (n,m) = (0,1), or
;    2) (n-1,m-2) in S.
;
; bottom-up: Define the set S to be the smallest set contained in N and
; satisfying the following 2 conditions:
;    1) (0,1) in S, and
;    2) if (n,m) in S, then (n+1,m+2) in S.
;
; rules of inference:
;                   (n,m) in S
;    ----------   --------------
;    (0,1) in S   (n+1,m+2) in S

; ------------------------------------------------------------------------------
; Exercise 1.1.4
;
; {(n,n^2) | n in N} = {(0,0), (1,1), (2,4), (3,9), ...}
;
; top-down: A pair of natural numbers (n,m) is in S if and only if
;    1) (n,m) = (0,0), or
;    2) (n-1,m-2n+1) in S.
;
; bottom-up: Define the set S to be the smallest set contained in N and
; satisfying the following 2 conditions:
;    1) (0,0) in S, and
;    2) if (n,m) in S, then (n+1,m+2n+1) in S.
;
; rules of inference:
;                    (n,m) in S
;    ----------   -----------------
;    (0,0) in S   (n+1,m+2n+1) in S
; ------------------------------------------------------------------------------
; Exercise 1.2

; ------------------------------------------------------------------------------
; Exercise 1.2.1
;
; {(n,7n+1) | n in N}

; ------------------------------------------------------------------------------
; Exercise 1.2.2
;
; {(n,2^n) | n in N}

; ------------------------------------------------------------------------------
; Exercise 1.2.3
;
; {(n,Fib(n),Fib(n+1)) | n in N}

; ------------------------------------------------------------------------------
; Exercise 1.2.4
;
; {(n,2n+1,n^2) | n in N}
; ------------------------------------------------------------------------------
; Exercise 1.3
; Find a set T of natural numbers such that 0 ∈ T
; and whenever n ∈ T, then n + 3 ∈ T, but T ≠ S,
; where S is the set defined in definition 1.1.2.
;
; T = N
; My explanation:
; S is the "smallest set" satisfys the two condition.
; Without "smallest" condition, adding any natural number into S makes a new set T
; and T ≠ S
; if and only if 1 and 2, then S
; if 1 and 2, then T
; ------------------------------------------------------------------------------
; Exercise 1.4
;
;    List-of-Int
; => (Int . List-of-Int)
; => (-7 . List-of-Int)
; => (-7 . (Int . List-of-Int))
; => (-7 . (3 . List-of-Int))
; => (-7 . (3 . (Int . List-of-Int)))
; => (-7 . (3 . (14 . List-of-Int)))
; => (-7 . (3 . (14 . ())))
; ------------------------------------------------------------------------------
; Exercise 1.5
;
; 1) e has the form Identifier:
;    There are 0 parentheses, therefore they are balanced.
; 2a) e has the form (lambda (Identifier) LcExp):
;     If LcExp has n opening/closing parentheses, e has n+2 opening/closing
;     parentheses, therefore they are balanced
; 2b) e has the form (LcExp LcExp):
;     If the first LcExp has n opening/closing parentheses and the second one m,
;     e has n+m+1 opening/closing parentheses.
; ------------------------------------------------------------------------------
; Exercise 1.6
;
; With a reversed order, nth-element would try to take the car of an empty list
; when called with a list which is 1 element too short.