; Exercise 1.36
; fixed-point adapted from SICP.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
           tolerance))

    (define (try guess)
        (newline)
        (display guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))

    (try first-guess))

(define (f x)
    (/ (log 1000)
       (log x)))

(newline)
(display "Undamped")
; 34 guesses
(define x (fixed-point f 10))
(newline)
(display x)

(define (f-damped x)
    (/ (+ (f x)
          x)
       2))

(newline)
(display "Damped")
; 11 guesses
(define x (fixed-point f-damped 10))
(newline)
(display x)
