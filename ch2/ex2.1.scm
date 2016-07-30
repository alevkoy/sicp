; Exercise 2.1
; gcd, print-rat, mul-rat, numer, denom adapted from SICP

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
    ; gcd as defined here produces a negative value for negative inputs
    (let ((g (abs (gcd n d)))
          (n-neg (< n 0))
          (d-neg (< d 0)))
        ; Value to multiply numerator and denominator by to normalize sign,
        ; depending on which of the numerator and denominator are negative
        (define multiplier (cond ((and n-neg d-neg) -1)
                                 (n-neg 1)
                                 (d-neg -1)
                                 (else 1)))

        (cons (/ (* multiplier n) g)
              (/ (* multiplier d) g))))

(define neg-one-quarter (make-rat 25 -100))
(define one-third (make-rat 1 3))
(print-rat (mul-rat neg-one-quarter one-third))
(print-rat (make-rat -39 -13))
