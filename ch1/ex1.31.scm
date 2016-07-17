; Exercise 1.31

; Recursive process
(define (product term a next b)
    (if (> a b)
        1
        (* (term a)
           (product term
                    (next a)
                    next
                    b))))

(define (factorial n)
    (define (term x) x)

    (define (next x) (+ x 1))

    ; Use a decimal number to produce fractional decimal
    ; results instead of rational numbers
    (product term
             2.0
             next
             n))

; There are different ways of counting the terms in the Wallis Formula series.
; In real math, that doesn't matter, because the series is infinite. Here, two
; of my terms form one term of the canonical Wallis Formula, so for the same
; number of terms, my procedure is, I suppose, half as precise as one using
; the canonical Wallis formula. No matter. I also do half as much work per term,
; so I can just use twice as many terms.
(define (pi n)
    (define (term x)
        (if (even? x)
            (/ x (+ x 1))
            (/ (+ x 1) x)))

    (define (next x) (+ x 1))

    (* 4.0 (product term
                  2
                  next
                  (+ n 1))))

(newline)
(display "factorial(12) ")
(display (factorial 12))
(newline)
(display "pi ")
(display (pi 200))

; Iterative process
(define (product term a next b)
    (define (iterate term a next b result)
        (if (> a b)
            result
            (iterate term
                     (next a)
                     next
                     b
                     (* (term a)
                        result))))
    (iterate term a next b 1))

(newline)
(display "factorial(12) ")
(display (factorial 12))
(newline)
(display "pi ")
(display (pi 200))
