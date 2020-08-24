#lang racket

(define (linear-combination a b x y)
  (+ (* a x) (b y)))

;(define (make-rat n d) (cons n d))
(define (make-rat n d)(let ((g (gcd n d)))
                        (cond ((or (and (< 0 n)(< 0 d)) (and (< n 0)(< 0 d))) (cons (/ (- n) g)(/ (- d) g)))
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
(define (cons-a x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car-a z) (z 0))
(define (cdr-a z) (z 1))

; exercise 2.4
(define (consi x y)
  (lambda (m) (m x y)))
(define (cari z)
  (z (lambda (p q) p)))
(define (cdri z)
  (z (lambda (p q) q)))

;exercise 2.5
(define (cons-exp a b)(* (expt 2 a)(expt 3 b)))
(define (car-exp z)
  (define (iter counter result)
    (if (not(= (remainder result 2) 0))
        counter
        (iter (+ counter 1) (/ result 2)
              )
        )
    )
  (iter 0 z)
  )

(define (cdr-exp z)
  (define (iter counter result)
    (if (not(= (remainder result 3) 0))
        counter
        (iter (+ counter 1) (/ result 3)
              )
        )
    )
  (iter 0 z)
  )

; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)(lambda (f) (lambda (x) ((a f)((b f) x)))))


;exercise 2.1.4
;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
; 2.10
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (if (= (upper-bound y) 0) (error "Division with zero is not allowed") (/ 1.0 (upper-bound y)))
                  (if (= (lower-bound y) 0) (error "Division with zero is not allowed")(/ 1.0 (lower-bound y))))))

; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; 2.9
(define (width z) (/ 2 (- (upper-bound z)(lower-bound z))))

;2.12
(define (make-center-width c p)
  (define perc (/ p 100))
  (make-interval (- c (* c perc) (+ c perc)))
  (define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))
  )

; 2.13
