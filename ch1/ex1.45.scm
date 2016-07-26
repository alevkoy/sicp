; Exercise 1.45
; fixed-point, average-damp adapted from SICP

(define (fixed-point f first-guess)
    (define tolerance 0.00001)

    (define (close-enough? v1 v2)
	    (< (abs (- v1 v2))
		   tolerance))

  (define (try guess)
    (let ((next (f guess)))
        (if (close-enough? guess next)
		    next
            (try next))))

  (try first-guess))

; I assume there is a way to define this for an arbitrary number of arguments
(define (average a b)
    (/ (+ a b) 2))

(define (average-damp f)
    (lambda (x)
        (average x (f x))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter count result)
        ; Doesn't make sense to repeat less than one time, but don't want
        ; to recurse forever if caller passes n=0
        (if (<= count 1)
            result
            (iter (-1+ count)
                  (compose f result))))

    (iter n f))

(define (nth-root x n)
    ; The number of average damps necessary appears to be log base 2
    ; of the highest power of 2 less than the exponent n
    (define num-damps (floor (/ (log n)
                                (log 2))))

    ; A function that average damps n times
    (define damp (repeated average-damp num-damps))

    (fixed-point (damp (lambda (y) (/ x
                                      (expt y (-1+ n)))))
                 1.0))

(newline)
(display "5th root of 32: ")
(display (nth-root 32 5))

(newline)
(display "6th root of 729: ")
(display (nth-root 729 6))
