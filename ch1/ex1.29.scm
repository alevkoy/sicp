; Exercise 1.29
; sum, integral, and cube taken from SICP

(define (simpson f a b n)
    ; Simpson's rule only produces accurate results for even numbers of
    ; intervals, so make sure that n is always even.
    (define even-n (+ n (remainder n 2)))

    (define h (/ (- b a) even-n))

    (define (term k)
        (define coef
            (cond ((= k 0) 1)
                  ((= k even-n) 1)
                  ((odd? k) 4)
                  (else 2)))

        (* coef (f (+ a
                      (* k h)))))

    (define (next k) (1+ k))

    (* (/ h 3.0) (sum term
                  0
                  next
                  even-n)))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))

    (* (sum f
            (+ a (/ dx 2.0))
            add-dx
            b)
        dx))

(define (cube x)
    (* x x x))

(define (output f a b divisor)
    (newline)
    (display "integral ")
    (display divisor)
    (display ": ")
    (display (integral f a b (/ 1.0 divisor)))
    (newline)
    (display "simpson ")
    (display divisor)
    (display ": ")
    (display (simpson f a b divisor))
    (newline))

(output cube 0 1 100)
(output cube 0 1 1000)
