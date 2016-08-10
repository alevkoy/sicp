; Exercise 2.5

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

; Return the number of times that base divides evenly into n
(define (divides-into base n)
    (if (= (remainder n base) 0)
        (1+ (divides-into base (/ n base)))
        0))

(define (car z)
    (divides-into 2 z))

(define (cdr z)
    (divides-into 3 z))

(define x (cons 5 7))
(newline)
(display (car x))
(display " ")
(display (cdr x))
