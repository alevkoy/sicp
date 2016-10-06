; Exercise 2.7
; add-interval, mul-interval, div-interval, and make-interval adapted from SICP

(define (add-interval x y)
    (make-interval (+ (lower-bound x)
                      (lower-bound y))
                   (+ (upper-bound x)
                      (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x)
                 (lower-bound y)))
          (p2 (* (lower-bound x)
                 (upper-bound y)))
          (p3 (* (upper-bound x)
                 (lower-bound y)))
          (p4 (* (upper-bound x)
                 (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b)
    (cons a b))

(define (upper-bound interval)
    (cdr interval))

(define (lower-bound interval)
    (car interval))

(define (print-interval i)
    (display "[")
    (display (lower-bound i))
    (display ",")
    (display (upper-bound i))
    (display "]"))

(define a (make-interval 3 5))
(define b (make-interval 2 7))
(newline)
(display "[3,5] + [2,7]: ")
(print-interval (add-interval a b))
(newline)
(display "[3,5] * [2,7]: ")
(print-interval (mul-interval a b))
(define c (make-interval 10 15))
(newline)
(display "[10,15] / [3,5]: ")
(print-interval (div-interval c a))

; Exercise 2.8

(define (sub-interval x y)
    (make-interval (- (lower-bound x)
                      (upper-bound y))
                   (- (upper-bound x)
                      (lower-bound y))))

(newline)
(display "[2,7] - [3,5]: ")
(print-interval (sub-interval b a))

; Exercise 2.9
; New definition of div-interval based on definition from SICP.

(define (div-interval x y)
    (if (or (and (< (lower-bound y) 0)
                 (> (upper-bound y) 0))
            (= (lower-bound y) 0)
            (= (upper-bound y) 0))
        (error "Division by 0"
                y
                (error-irritant/noise " within procedure")
                "div-interval"
                (error-irritant/noise "."))
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

(newline)
(print-interval (div-interval c a))
(define d (make-interval -1 1))
; Will result in division by 0
; Don't interfere with further testing
; (print-interval (div-interval c d))

; Exercise 2.11

; Multiply two intervals while avoiding unnecessary multiplications. The
; algorithm differs depending on whether each of the bounds is nonnegative.
; There are 9 possible combinations. If a bound is 0, there might be
; multiple possible ways to get the product, but we only need to try the
; first one that works.
(define (mul-interval x y)
    ; Populate a list based on whether each bound in 2 intervals is
    ; nonnegative.
    (define (intervals2signs x y)
        (list (>= (lower-bound x) 0)
              (>= (upper-bound x) 0)
              (>= (lower-bound y) 0)
              (>= (upper-bound y) 0)))

    (let ((signs (intervals2signs x y))
          (mi make-interval)
          (lbx (lower-bound x))
          (ubx (upper-bound x))
          (lby (lower-bound y))
          (uby (upper-bound y)))
        ; Bounds of product depend on signs of bounds of factors
        ;       1                   +  +  +  +
        (cond ((equal? signs (list #t #t #t #t))
               (mi (* lbx lby) (* ubx uby)))
              ; 2                   +  +  -  +
              ((equal? signs (list #t #t #f #t))
               (mi (* ubx lby) (* ubx uby)))
              ; 3                   +  +  -  -
              ((equal? signs (list #t #t #f #f))
               (mi (* ubx lby) (* lbx uby)))
              ; 4                   -  +  +  +
              ((equal? signs (list #f #t #t #t))
               (mi (* lbx uby) (* ubx uby)))
              ; 5                   -  +  -  +
              ((equal? signs (list #f #t #f #t))
               (mi (min (* lbx uby) (* lby ubx))
                   (max (* lbx lby) (* ubx uby))))
              ; 6                   -  +  -  -
              ((equal? signs (list #f #t #f #f))
               (mi (* ubx lby) (* lbx lby)))
              ; 7                   -  -  +  +
              ((equal? signs (list #f #f #t #t))
               (mi (* lbx uby) (* ubx lby)))
              ; 8                   -  -  -  +
              ((equal? signs (list #f #f #f #t))
               (mi (* lbx uby) (* lbx lby)))
              ; 9                   -  -  -  -
              ((equal? signs (list #f #f #f #f))
               (mi (* ubx uby) (* lbx lby)))
        )))

(define pospos (make-interval 3 5))
(define negpos (make-interval -4 2))
(define negneg (make-interval -5 -2))
(define zeropos (make-interval 0 3))
(define negzero (make-interval -2 0))

; Compare the result of an operation to its expected result
(define (test-op op opsym a b expected)
    (define result (op a b))
    (newline)
    (print-interval a)
    (display " ")
    (display opsym)
    (display " ")
    (print-interval b)
    (display ": ")
    (print-interval result)
    (display " ")
    (if (equal? result expected)
        (display "PASS")
        (display "FAIL")))

(define (test-mul a b expected)
    (test-op mul-interval "*" a b expected))

; Just pospos
(test-mul pospos pospos (make-interval 9 25))
; and negpos
(test-mul pospos negpos (make-interval -20 10))
(test-mul negpos pospos (make-interval -20 10))
(test-mul negpos negpos (make-interval -8 16))
; and negneg
(test-mul pospos negneg (make-interval -25 -6))
(test-mul negneg pospos (make-interval -25 -6))
(test-mul negpos negneg (make-interval -10 20))
(test-mul negneg negpos (make-interval -10 20))
(test-mul negneg negneg (make-interval 4 25))
; and zeropos
(test-mul pospos zeropos (make-interval 0 15))
(test-mul zeropos pospos (make-interval 0 15))
(test-mul negpos zeropos (make-interval -12 6))
(test-mul zeropos negpos (make-interval -12 6))
(test-mul negneg zeropos (make-interval -15 0))
(test-mul zeropos negneg (make-interval -15 0))
(test-mul zeropos zeropos (make-interval 0 9))
; and negzero
(test-mul pospos negzero (make-interval -10 0))
(test-mul negzero pospos (make-interval -10 0))
(test-mul negpos negzero (make-interval -4 8))
(test-mul negzero negpos (make-interval -4 8))
(test-mul negneg negzero (make-interval 0 10))
(test-mul negzero negneg (make-interval 0 10))
(test-mul zeropos negzero (make-interval -6 0))
(test-mul negzero zeropos (make-interval -6 0))
(test-mul negzero negzero (make-interval 0 4))

; Exercise 2.12
; center adapted from SICP.

(define (center i)
    (/ (+ (lower-bound i)
		  (upper-bound i))
	   2))

(define (make-center-percent c p)
    (let ((w (* c p)))
        (make-interval (- c w) (+ c w))))

(define (percent i)
    (let ((c (center i))
          (ub (upper-bound i)))
        (/ (- ub c)
           c)))

(define e (make-center-percent 100 .05))
(newline)
(print-interval e)
(display ": center = ")
(display (center e))
(display ", percent = ")
(display (percent e))

; Exercise 2.13
; par1 and par2 adapted from SICP.

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one
                      (add-interval (div-interval one r1)
                                    (div-interval one r2)))))
