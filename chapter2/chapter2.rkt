#lang racket
(define nil '())

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
; exercise 2.7
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
; exercise 2.10
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (if (= (upper-bound y) 0) (error "Division with zero is not allowed") (/ 1.0 (upper-bound y)))
                  (if (= (lower-bound y) 0) (error "Division with zero is not allowed")(/ 1.0 (lower-bound y))))))

; exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; exercise 2.9
(define (width z) (/ 2 (- (upper-bound z)(lower-bound z))))

; exercise 2.12
(define (make-center-width c p)
  (define perc (/ p 100))
  (make-interval (- c (* c perc) (+ c perc))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (c-width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; 2.2
(define one-through-four (list 1 2 3 4))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)(- n 1))
      )
  )

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))
      )
  )

(define (lengthi items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0)
  )

(define odds (list 1 3 5 7))

;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1)(append (cdr list1) list2))
;      )
;  )

; exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))
      ))

;exercise 2.18
(define (reverse items)
  (define (rev-iter dest src)
    (if (null? src)
        dest
        (rev-iter (cons (car src) dest) (cdr src))))
  (rev-iter (list) items)
  )

; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (except-first-denomination z)(cdr z))
(define (first-denomination z)(car z))
(define (no-more? z)(null? z))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

; exercise 2.20
(define (same-parity x . l)
  (define (parity l)
    (cond ((null? l) (list))
          ((= (remainder x 2) (remainder (car l) 2)) (cons (car l) (parity (cdr l))))
          (else (parity (cdr l)))

          ))
  (parity l))


(define (scale-list-o items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))


(define (map-own proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))
      )
  )

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items)
  )

;exercise 2.21
(define (square x)(* x x))
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))(square-list (cdr items))))
  )

(define (square-list-map items)
  (map (lambda (x)(* x x)) items)
  )

;exercise 2.23
(define (for-each-own proc list)
  (cond ((not (null? list)) (proc (car list))(for-each-own proc (cdr list)))
        ))

;2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

; exercise 2.24
;'(1 (2 (3 4)))
; o-1
;  -o-2
;    -o-3
;      -4
; [1|[2|[3|4|nil]|nil]|nil]

;exercise 2.25
(define (pick-seven-first items)
  (define (iter list)
    (if (pair? (car list))
        (car (cdr (car list)))
        (iter (cdr list))
     )
  )
  (iter items)
)
(define (pick-seven-second items)
  (car (car items))
  )

(define (pick-seven-third items)
  (if (pair? (cdr items))
      (car (cdr items))
      (pick-seven-third (cdr items))
      )
  )

