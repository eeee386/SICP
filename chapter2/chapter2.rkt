#lang racket

(define (linear-combination a b x y)
  (+ (* a x) (b y)))


(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))


;(define (make-rat n d) (cons n d))
(define (make-rat n d)(let ((g (gcd n d)))
                        (cond ((or (and (< 0 n)(< 0 d)) (and (< n 0)(< 0 d))) (cons (/ (- n) g)(/ (-d) g)))
                              (else (cons (/ n g)(/ d g)))
                        )))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  )

(define (add-rat x y)
  (make-rat (+ (* (numer x)(denom y))
               (* (numer y)(denom x)))
            (* (denom x)(denom y)))
)

(define (sub-rat x y)
  (make-rat (- (* (numer x)(denom y))
            (* (numer y)(denom x)))
            (* (denom x)(denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

; exercise 2.2
(define (average x y) (/ (+ x y) 2))
(define (make-point a b)(cons a b))
(define (x-point p)(car p))
(define (y-point p)(cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
 )


(define (make-segment p1 p2)(cons p1 p2))
(define (start-segment x) (car x))
(define (end-segment x)(cdr x))
(define (midpoint-segment s)
  (make-point
   (average (x-point start-segment) (x-point end-segment))
   (average (y-point start-segment) (y-point end-segment))))

;exercise 2.3

;2.1.3

