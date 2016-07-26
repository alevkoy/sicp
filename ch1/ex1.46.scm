; Exercise 1.46
; close-enough? adapted from SICP's fixed-point; improve and average adapted
; from SICP's sqrt.

(define (close-enough? v1 v2)
    (define tolerance 0.00001)

    (< (abs (- v1 v2))
       tolerance))

(define (iterative-improve improve good-enough?)
    (define (iter guess)
        (let ((next (improve guess)))
            (if (good-enough? guess)
                next
                (iter next))))

    iter)

(define (sqrt x)
    (define (improve guess)
        (define (average x y)
            (/ (+ x y) 2))

        (average guess
                 (/ x guess)))

    ((iterative-improve improve
                        (lambda (guess)
                            (close-enough? guess
                                           (improve guess))))
        1.0))

(define (fixed-point f)
    ((iterative-improve f
                        (lambda (guess)
                            (close-enough? guess
                                           (f guess))))
        1.0))

(newline)
(display "Square root of 4: ")
(display (sqrt 4))
(newline)
(display "Square root of 49: ")
(display (sqrt 49))
(newline)
(display "Square root of 625: ")
(display (sqrt 625))

(newline)
(display "Fixed point of cosine: ")
(display (fixed-point cos))
(newline)
(display "Fixed point of y = sin y + cos y: ")
(display (fixed-point (lambda (y) (+ (sin y) (cos y)))))
