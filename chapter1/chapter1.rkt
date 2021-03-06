#lang racket
(#%require (only racket/base current-milliseconds))
(define (runtime) (current-milliseconds))
; 1.1

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
; Because of applicative-order sqrt-iter will be called infinite times,
; new-if applicative eval while the if is normal eval

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; exercise 1.7
; fix for small numbers, I could not recreate the problem for large numbers
(define (good-enough?2 guess x)
  (< (abs (- (square guess) x)) (if (< x 1) (* 0.001 x) 0.001)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter(improve guess x) x)))


(define (sqrt x)
  (sqrt-iter 1.0 x))




; exercise 1.8
(define (improveC guess x)(/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough?C guess x)
  (< (abs (- (square guess) x)) (if (< x 1) (* 0.001 x) 0.001)))

(define (cube-root-iter guess x)
  (if (good-enough?C guess x) guess (cube-root-iter (improveC guess x) x)))

(define (cube-root x)(cube-root-iter 1.1 x))


(define (sqrt-block x)
  (define (good-enough? guess)(< (abs (- (square guess) x)) (if (< x 1) (* 0.001 x) 0.001)))
  (define (improve guess)(average guess (/ x guess)))
  (define (sqrt-iter guess)(if (good-enough? guess) guess (sqrt-iter (improve guess))))
  (sqrt-iter 1.0)
  )


;1.2
(define (factorial n)(if (= n 1) 1 (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product) (+ counter 1) (max-count))))
  (fact-iter 1 1 n)
  )

(define (factorial-iter-local n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1)
  )

;1.9 1. recursive process, iterative process

;1.10 Ackermann function
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)(A x (- y 1))))
    ))

;1. 1024 (2^10)
;2.-3. 65536 (2^(2^4))
;4. 2n
;5. 2^n
;6. 2^(2^n)

;1.2.2
; Fibonacci
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2)))
          ))
  )

(define (fib-iter n)
  (define (iter a b count) (if (= count 0) b (iter (+ a b) a (- count 1))))
  (iter 1 0 n)
  )

; Counting change
(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond
      ((= kinds-of-coins 1) 1)
      ((= kinds-of-coins 2) 5)
      ((= kinds-of-coins 3) 10)
      ((= kinds-of-coins 4) 25)
      ((= kinds-of-coins 5) 50)
      ))
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else
           (+
            (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))
           ))
    )(cc amount 5))

; exercise 1.11
(define (f11 n)
  (if (< n 3)
      n
      (+
       (f11 (- n 1))
       (* 2 (f11 (- n 2)))
       (* 3 (f11 (- n 3)))
       )
      )
  )

(define (f11i n)
  (define (f-iter a b c counter)
    (if (counter < 3)
        a
        (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3) n (f-iter 2 1 0 n))
  )


(define (pascal-tri row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= row col)) 1)
        (else (+ (pascal-tri (- row 1) (- col 1)) (pascal-tri (- row 1) col))      
              )))


; 1.2.4
(define (expt b n) (if (= n 0) 1 (* b (expt b (- n 1)))))

(define (expti b n)
  (define (expt-iter b counter product)
    (if (= counter 0) product (expt-iter b (- counter 1) (* b product)))
    )(expt-iter b n 1))


(define (fast-expt b n)
  (define (even? n)(= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square(fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))
        )
  )
; 1.16
(define (fast-expti b n)
  (define (even? n)(= (remainder n 2) 0))
  (define (iter product counter)
    (cond ((= counter 0) product)
          ((even? counter) (iter (square product) (/ counter 2)))
          (else (iter (* b product) (- counter 1)))
          )
    )
  (iter 1 n)
  )

; 1.17
(define (mult b n)
  (define (even? n)(= (remainder n 2) 0))
  (define (double k) (+ k k))
  (define (halve k) (/ k 2))
  (cond ((= n 0) 0)
        ((even? n) (mult (double b) (halve n)))
        (else (+ b (mult b (- n 1))))
        )
  )

; 1.18
(define (multi a b)
  (define (even? n)(= (remainder n 2) 0))
  (define (double k) (+ k k))
  (define (halve k) (/ k 2))
  (define (iter sum counter)
    (cond
      ((= counter 0) sum)
      ((even? counter) (iter (double sum) (halve counter)))
      (else (iter (+ sum a) (- counter 1)))
      )
    )
  (iter 0 b)
  )

; 1.19
(define (fibi n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q)) ; compute p'
                     (+ (* 2 q p) (* q q)) ; compute q′
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))



; 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.2.6
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; 1.22

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes start-range end-range)
  (if (even? start-range)
      (search-for-primes (+ 1 start-range) end-range)
      (cond ((> start-range end-range)
             (newline) (display "done"))
            (else (timed-prime-test start-range)
                  (search-for-primes (+ 2 start-range) end-range)))))

; 1.3.1
(define (cube x) (* x x x))
(define (sum-integers a b)(if (> a b) 0 (+ a (sum-integers (+ a 1) b))))
(define (sum-cubes a b) (if (> a b) 0 (+ (cube a)(sum-cubes (+ a 1) b))))
(define (pi-sum a b)(if (> a b) 0 (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)(if (> a b) 0 (+ (term a)(sum term (next a) next b))))

(define (inc n)(+ n 1))
(define (sum-cubes-hof a b)(sum cube a inc b))

(define (identity x) x)
(define (sum-integers-hof a b) (sum identity a inc b))

(define (pi-sum-hof a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b)
  )

(define (integral f a b dx)
  (define (add-dx x)(+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)
  )

(define (sum-n term a next b counter inc-counter n)
   (if (> counter n) 0 (+ (term a)(sum-n term (next a) next b (inc-counter counter) inc-counter n))))

(define (sumi-n term a next b counter inc-counter n)
  (define (iter a result counter) 
    (if (> counter n)
        result
        (iter (next a) (+ (term a) result) (inc-counter counter))))
  (iter a 0 counter)
  )

; 1.29 Simpson's rule
(define (simpson-integer f a b n)
  (define counter 1)
  (define (inc-c counter)(+ counter 2))
  (define h (/ (- b a) n))
  (define cons (/ h 3))
  (define (sf x) (+ (* 4 (f x)) (* 2 (f (+ x h)))))
  (define (inc-x x) (+ x (* 2 h)))
  (define series (+ (f a) (sumi-n sf (inc-x a) inc-x b counter inc-c n)))
    (if (= (remainder n 2) 0) (* cons (+ series (* 2 (+ a (* n h))))) (* cons series))
  )

; 1.30
(define (sumi term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (+ (term a) result))))
  (iter a 0)
  )

(define (sum-cubes-hofi a b)(sumi cube a inc b))

;1.31
; a
(define (product term a next b)
  (if (> a b) 1 (* (term a)(product term (next a) next b))))

; b
(define (producti term a next b)
  (define (iter a result)
  (if (> a b) result (iter (next a) (* (term a) result))))
  (iter a 1)
  )

(define (factorial2r a b)
  (product identity a inc b))

(define (factorial2i a b)
  (producti identity a inc b))

(define (pi-d4 b)
  (define (next a) (+ a 2))
  (define (term a) (/ (* a (+ a 2))(* (+ a 1)(+ a 1))))
  (producti term 2.0 next b)
  )

; exercise 1.32
(define (accumulate combiner init-value term a next b)
  (if (> a b)
      init-value
      (combiner
       (term a)
       (accumulate combiner init-value term (next a) next b)))
  )

(define (accumulatei combiner init-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
  (iter a init-value)
  )

; 1.33
(define (filtered-accumulate combiner init-value term a next b predicate)
  (if (> a b)
      init-value
      (if (predicate a)
       (combiner
        (term a)
        (filtered-accumulate combiner init-value term (next a) next b predicate))
       (filtered-accumulate combiner init-value term (next a) next b predicate)
       )
      )
  )

(define (filtered-accumulatei combiner init-value term a next b predicate)
  (define (iter a result)
  (if (> a b)
      result
      (if (predicate a)
          (iter (next a) (combiner (term a) result))
          (iter (next a) result)
          )
      ))
  (iter a init-value)
  )

;(filtered-accumulate + 0 square 1 inc 6 prime?)

(define (sum-of-relative-primes-to-n n)
  (define (predicate x) (gcd x n))
  (filtered-accumulatei * 1 identity 1 inc n predicate)
  )

(define (pi-sum-lam a b)
  (sum
   (lambda (x) (/ 1.0 (* x (+ x 2))))
   a
   (lambda (x) (+ x 4))
   b)
  )

(define (integral-lam f a b dx)
  (* (sum
     f
     (+ a (/ dx 2.0))
     (lambda (x)(+ x dx))
     b
     )
   dx)
  )

(define (f-let x y)
  (let (
        (a (+ 1 (* x y)))
        (b (- 1 y))
        )
  (+ (* x (square a))(* y b)(* a b))
  )
  )

(define (close-enough? x y)(< (abs (- x y)) 0.001))
(define (positive? a)(> a 0))
(define (negative? a)(< a 0))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value) (search f neg-point midpoint))
                ((negative? test-value) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let (
        (a-value (f a))
        (b-value (f b))
        )
    (cond ((and (negative? a-value)(positive? b-value)) (search f a b))
          ((and (negative? b-value)(positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b))
          ))
  )

; (half-interval-method sin 2.0 4.0)
;(half-interval-method (lambda (x) (- (* x x x)(* 2 x) 3)) 1.0 2.0)

(define tolerance 0.00001)
(define (close-enough? v1 v2)(< (abs (- v1 v2)) tolerance))
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next)
          )
      ))
  (try first-guess)
  )


; (fixed-point cos 1.0)
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;exercise 1.35
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;exercise 1.36
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)


;exercise 1.36

(define (sqrt-f x)(fixed-point (lambda (y) (average y (/ x y))) 1.0))


; exercise 1.37
(define (cont-frac n d k counter)
    (if (> counter k)
        0
        (/ (n counter) (+ (d counter) (cont-frac n d k (+ counter 1)))))
    )

(define (cont-fraci n d k init-value)
  (define (iter counter result)
    (if (= counter init-value)
        result
        (iter
         (- counter 1)
         (/ (n counter) (+ (d counter) result)))
    )
    )
  (iter k 1.0)
  )
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 16 1)
; (cont-fraci (lambda (i) 1.0) (lambda (i) 1.0) 16 1)

; exercise 1.38
; won't with iterative because it goes from the back, could be fixed
(define (euler-exp k)
  (let ((dr 0))
  (cont-frac
   (lambda (i) 1.0)
   (lambda (i)
     (display i)
     (newline)
     (if (= (remainder i 3) 2)
         ((lambda ()
           (set! dr (+ dr 2))
            dr))
         1
         )) k 1)))

; exercise 1.39

(define (tan-cf x k)
  (/ x (+ 1 (cont-frac
             (lambda (i) (* x x -1))
             (lambda (k) (- (* 2.0 k) 1))
             k
             2.0
             )))
  )


; 1.3.4
(define (average-damp f)(lambda (x)(average x (f x))))

(define (sqrt-fixed x)(fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(define (cube-root-fixed x)(fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))


(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))


(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x) (newton-method (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess) (fixed-point (transform g) guess))
(define (sqrt-trans x) (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))
(define (sqrt-newton-trans x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

; 1.40
(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;1.41
(define (double-proc proc)
  (lambda (x) (proc (proc x)))
  )

; (((double-proc (double-proc double-proc)) inc) 5)

;1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43
(define (repeated f n)
  (define (iter n result)
    (if (> 1 n)
        result
        (iter (- n 1) (f result))
        )
    )
  (lambda (x) (iter n x))
  )


; 1.44
(define (average-three a b c) (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x) (average-three (f (- x dx)) (f x) (f (+ x dx)))))
;(((repeated smooth 5) square) 2)

; 1.45
;(define (sqrt-fixed x)(fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
;(floor (log 3 2))

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log n 2))) (lambda (y) (/ x (expt y (- n 1)))))
   1.0
   )
  )


;1.46
(define (iterative-improve is-good-enough? improve-guess)
  (define (iter guess x)
        (if (is-good-enough? guess x)
            guess
            (iter (improve-guess guess x) x)
            ))
  (lambda (guess x) (iter guess x))
  )

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter(improve guess x) x)))

(define (sqrt-iter-imp guess x)
  ((iterative-improve good-enough? improve) guess x)
  )

;1.46 fixed point missing