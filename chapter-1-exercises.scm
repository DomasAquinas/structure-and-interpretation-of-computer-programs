#|
 | Structure and Interpretation of Computer Programs
 |
 | Exercises for Chapter 1
 |#

#|
 | Exercise 1-1
 |#

 ; 10 -> 10

 ; (+ 5 3 4) -> 12

 ; (- 9 1) -> 8

 ; (/ 6 2) -> 3

 ; (+ (* 2 4) (- 4 6)) -> 6

 ; (define a 3) -> _

 ; (define b (+ a 1)) -> _

 ; (+ a b (* a b)) -> 19

 ; (= a b) -> #f

 ; (if (and (> b a) (< b (* a b)))
 ;     b
 ;     a) -> 4

 ; (cond ((= a 4) 6)
 ;       ((= b 4) (+ 6 7 a))
 ;       (else 25)) -> 16

 ; (+ 2 (if (> b a) b a)) -> 6

 ; (* (cond ((> a b) a)
 ;          ((< a b) b)
 ;          (else -1))
 ;    (+ a 1)) -> 16


#|
 | Exercise 1-2
 |#

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


#|
 | Exercise 1-3
 |#
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum-of-greater-two-squares x y z)
        (cond ((or (> x y z) (> y x z)) (sum-of-squares x y))
              ((or (> x z y) (> z x y)) (sum-of-squares z x))
              ((or (> y z x) (> z y x)) (sum-of-squares y z))))

#|
 | Exercise 1-4
 |#
(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))

; This procedure uses a compound expression as an operator to perform a
; conditional operation on a and b. If b > 0, then a and b are added. If b < 0,
; then it is subtracted from a, effectively making this an addition of its
; absolute value in either case. Hence the name "a-plus-abs-b."

#|
 | Exercise 1-5
 |#

; Proposed test:
(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y))

(test 0 p)

; Observed behavior with applicative-order evaluation:
; The interpreter will try to evaluate the arguments and, finding that p is
; defined as itself, it will recurse forever and never return...

; Observed behavior with normal-order evaluation:
; The interpreter will expand 'test' first by evaluating the predicate,
; expanding it to (= 0 0) and finding that to be #t. It will therefore
; evaluate only the consequent, 0, and return that.

#|
 | Exercise 1-6
 |#

; Because Alyssa provides the clauses as arguments, applicative-order
; evaluation would cause the arguments to be evaluated before their insertion
; into the full expression. Because the recursion assertion is included as an
; argument, it will therefore be called recursively before ever being put into
; context with the loop termination. Alyssa's program will hang in infinite
; recursion.

#|
 | Exercise 1-7
 |#

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

; This will fail for small numbers because when 0.001 is on a similar order
; of magnitude to the original number, the tolerable error in the square root
; procedure will be significant and the calculated square root will not be
; accurate:

(sqrt-iter 0.05 0.001) ; -> 0.035

(square 0.035) ; -> 0.001225... this is over 20% error!

; For large enough numbers, the computer will not have enough space in the
; binary representation of the number to reach the decimal precision of 0.001.
; Therefore, the check will hang, as good-enough? cannot evaluate to true.

; (sqrt-iter 123456789123456789.0 123456789123456789123456789123456789.0)
;   -> hangs in infinite recursion

; The alternative:
(define (fractional-good-enough? guess x)
    (< (abs(- (square guess) x)) (* x 0.001)))

(define (fractional-sqrt-iter guess x)
    (if (fractional-good-enough? guess x)
        guess
        (fractional-sqrt-iter (improve guess x)
                              x)))

; The small number:
(fractional-sqrt-iter 0.05 0.001) ; -> 0.03162277660444136

(square 0.0316227766044136) ; -> 0.001000... much better!

; The large number:
(fractional-sqrt-iter 123456789123456789.0 123456789123456789123456789123456789.0)
;   -> 3.5136976... e+17

(square 3.513697606e+17) ; -> 1.234607... e+35 - within the stated error!

#|
 | Exercise 1-8
 |#

(define (cube x) (* x x x))

(define (improve-cube guess x)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))

(define (cube-good-enough? guess x)
    (< (abs(- (cube guess) x)) (* x 0.001)))

(define (cube-root-iter guess x)
    (if (cube-good-enough? guess x)
        guess
        (cube-root-iter (improve-cube guess x)
                        x)))

(cube-root-iter 2.0 10.0) ; -> 2.1545036...

(cube 2.1545036) ; -> 10.00095...


#|
 | Exercise 1-9
 |#

; Case I:

; (define (+ a b)
;     (if (= a 0)
;         b
;         (inc (+ (dec a) b))))

; (+ 3 2) expands to...
; (+ 3 2)
; (+ (+ 2 2) 1)
; (+ (+ (+ 1 2) 1) 1)
; (+ (+ (+ (+ 0 2) 1) 1) 1)
; This is a recursive process.

; Case II:

; (define (+ a b)
;     (if (= a 0)
;         b
;         (+ (dec a) (inc b))))

; (+ 3 2) expands to...
; (+ 3 2)
; (+ 2 3)
; (+ 1 4)
; (+ 0 5)
; This is an iterative process.


#|
 | Exercise 1-10
 |#

(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1)
                   (A x (- y 1))))))

(A 1 10) ; -> (A 1 10)
         ;    (A 0 (A 1 9))
         ;    (A 0 (A 0 (A 1 8)))
         ;    (A 0 (A 0 (A 0 (A 1 7))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (2))))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (4)))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (8))))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (16)))))))
         ;    (A 0 (A 0 (A 0 (A 0 (A 0 (32))))))
         ;    (A 0 (A 0 (A 0 (A 0 (64)))))
         ;    (A 0 (A 0 (A 0 (128))))
         ;    (A 0 (A 0 (256)))
         ;    (A 0 (512))
         ;    (1024)
         ;    1024 - correct!

(A 2 4) ; -> (A 2 4)
        ;    (A 1 (A 2 3))
        ;    (A 1 (A 1 (A 2 2)))
        ;    (A 1 (A 1 (A 1 (A 2 1))))
        ;    (A 1 (A 1 (A 1 (2))))
        ;    (A 1 (A 1 (A 0 (A 1 1))))
        ;    (A 1 (A 1 (4)))
        ;    (A 1 (A 0 (A 1 3)))
        ;    (A 1 (A 0 (A 0 (A 1 2))))
        ;    (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
        ;    (A 1 (16))
        ;    = 2^16 = 65536 - correct!

(A 3 3) ; -> (A 3 3)
        ;    (A 2 (A 3 2))
        ;    (A 2 (A 2 (A 3 1)))
        ;    (A 2 (A 2 2))
        ;    (A 2 (A 1 (A 2 1)))
        ;    (A 2 (A 1 2))
        ;    (A 2 (A 0 (A 1 1)))
        ;    (A 2 4)
        ;    65536 - correct!

; (define (f n) (A 0 n)) computes 2*n
; (define (g n) (A 1 n)) computes 2^n, n > 0
; (define (h n) (A 2 n)) computes 2^2^... with n 2's, n > 0