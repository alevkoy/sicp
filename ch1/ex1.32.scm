; Exercise 1.32

; Recursive process
(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner
                              null-value
                              term
                              (next a)
                              next
                              b))))

(define (sum term a next b)
    (accumulate +
                0
                term
                a
                next
                b))

(define (product term a next b)
    (accumulate *
                1
                term
                a
                next
                b))

(define (inc x) (+ x 1))

(define (identity x) x)

(newline)
(display "Sum from 1 to 5: ")
(display (sum identity 1 inc 5))
(newline)
(display "Product from 1 to 5: ")
(display (product identity 1 inc 5))

; Iterative process
(define (accumulate combiner null-value term a next b)
    (define (iterate term a next b result)
        (if (> a b)
            result
            (iterate term
                     (next a)
                     next
                     b
                     (combiner (term a)
                               result))))

    (iterate term a next b null-value))

(newline)
(display "Sum from 1 to 5: ")
(display (sum identity 1 inc 5))
(newline)
(display "Product from 1 to 5: ")
(display (product identity 1 inc 5))
