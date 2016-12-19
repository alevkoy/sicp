; Exercise 2.20

(define (same-parity first . rest)
    ; Ideally, this would be overloaded on type, but if Scheme supports
    ; polymorphism, I haven't learned about it yet.
    (define (same-parity-first num)
        (= (remainder num 2)
           (remainder first 2)))

    (define (internal numbers)
        (if (= (length numbers) 0)
            '()
            (let ((rest-result (internal (cdr numbers)))
                  (current (car numbers)))
                 (if (same-parity-first current)
                      (cons current rest-result)
                      rest-result))))

    (internal (cons first rest)))

(newline)
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
