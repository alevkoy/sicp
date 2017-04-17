; Exercise 2.39
; Skeletons of reverse adapted from SICP.

(define (reverse sequence)
    (fold-right (lambda (current result)
                    (append result (list current)))
                '()
                sequence))

(newline)
(display (reverse (list 1 2 3 4)))

(define (reverse sequence)
    (fold-left (lambda (result current)
                   (cons current result))
               '()
               sequence))

(newline)
(display (reverse (list 1 2 3 4)))
