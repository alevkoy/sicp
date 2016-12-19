; Exericse 2.21

; Solution based on skeleton functions provided in prompt

(define (square-list items)
    (if (null? items)
        '()
        (cons (* (car items) (car items))
              (square-list (cdr items)))))

(newline)
(display (square-list (list 1 2 3 4)))

(define (square-list items)
    (map (lambda (x) (* x x))
         items))

(newline)
(display (square-list (list 1 2 3 4)))
