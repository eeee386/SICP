#lang racket
;1.1.4, 1.1.5
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a)(sum-of-squares (+ a 1) (* a 2)))

;1.1.6
;1.
(define (abs1 x)
  (cond
    ((> x 0) x)
    ((< x 0)(- x))
    ((= x 0) 0)
    ))

;2.
(define (abs2 x)
  (cond
    ((< x 0)(- x))
    (else x))
  )

;3
(define (abs3 x)(if (< x 0)(- x) x))

;and or not
;(and (> x 5)(< x 10))
(define (>= x y) (or (> x y)(= x y)))
(define ('>= x y) (not (> x y)))

; exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))(* 3 (- 6 2)(- 2 7)))

; exercise 1.3
(define (sum-of-squares-of-the-two-larger x y z)(
   cond ((and (< x y)(< x z)) (sum-of-squares y z))
        ((and (< y z)(< y x)) (sum-of-squares x z))
        ((and (< z x)(< z y)) (sum-of-squares x y))))

;exercise 1.4
(define (a-plus-abs-b a b)((if (> b 0) + -) a b))


;1.1.7
(define (improve guess x)(average guess (/ x guess)))

(define (average x y)(/ (+ x y) 2))

;exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)(else else-clause)))
; Because of applicative-order sqrt-iter will be called infinite times
;(new-if applicative eval) while the is is normal eval

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter(improve guess x) x)))


(define (sqrt x)
  (sqrt-iter 1.0 x))




        
