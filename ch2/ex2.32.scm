; Exercise 2.32
; subsets adapted from template in SICP.

(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest
                    (map (lambda (partial-subset)
                            (cons (car s) partial-subset))
                         rest)))))

(define set (list 1 2 3))
(newline)
(display (subsets set))
