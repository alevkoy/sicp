; Exercise 2.27

(define (deep-reverse items)
    (define (reverse-iter from to)
        (cond ((null? from) to)
              ; Descend into elements that are lists (pairs)
              ((not (pair? (car from))) (reverse-iter (cdr from)
                                                      (cons (car from) to)))
              ; Otherwise the same as reverse
              (else (reverse-iter (cdr from)
                                  (cons (deep-reverse (car from))
                                        to)))))
    (reverse-iter items '()))

(define x (list (list 1 2) (list 3 4)))
(newline)
(display (reverse x))
(newline)
(display (deep-reverse x))
