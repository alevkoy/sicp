; Exercise 2.6
; zero and add-1 adapted from SICP

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

; Convert a Church numeral into a regular numeral so that it can be displayed
(define (test-church c)
    ((c 1+) 0))

; A Church numeral represents a number as a procedure that applies f to its
; input as many times as the number.
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add-church a b)
    ; The result of ((b f) x) is the application of f repeated b times
    ; with a starting input of x. Applying (a f) to this result causes
    ; f to be applied a more times (using a and b loosely to refer to
    ; the numbers they represent). The total number of times that f is
    ; applied is a + b. Therefore, the result is a procedure of f
    ; returning a procedure of x that applies f to x (a + b) times.
    (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; My first attempt at add. Apply (b f) to x a times.
(define (mul-church a b)
    (lambda (f) (a (b f))))

(newline)
(display "one: ")
(display (test-church one))
(newline)
(display "two: ")
(display (test-church two))
(newline)
(display "one plus two: ")
(display (test-church (add-church one two)))
