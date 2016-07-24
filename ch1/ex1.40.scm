; Exercise 1.40
; fixed-point, deriv, newton-transform, newtons-method, square, cube adapted
; from SICP

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	  (< (abs (- v1 v2))
		 tolerance))

  (define (try guess)
    (let ((next (f guess)))
        (if (close-enough? guess next)
		    next
            (try next))))

  (try first-guess))

(define dx 0.00001)

(define (deriv g)
    (lambda (x)
	    (/ (- (g (+ x dx)) (g x))
           dx)))

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x)
                ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (square x)
    (* x x))

(define (cube x)
    (* x x x))

(define (cubic a b c)
    (lambda (x) (+ (cube x)
                   (* a (square x))
                   (* b x) c)))

(define test (cubic 2 3 4))
(newline)
(display (test (newtons-method test 1)))
(define test (cubic 5 5 7))
(newline)
(display (test (newtons-method test 1)))
(define test (cubic 8 9 10))
(newline)
(display (test (newtons-method test 1)))
(define test (cubic 11 12 13))
(newline)
(display (test (newtons-method test 1)))
