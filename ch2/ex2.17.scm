; Exercise 2.17

(define (last-pair l)
    (if (null? (cdr l))
        l
        (last-pair (cdr l))))

(define l (list 23 72 149 34))
(newline)
(display l)
(newline)
(display (last-pair l))
