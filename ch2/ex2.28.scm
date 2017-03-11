; Exercise 2.28

(define (fringe tree)
    (define (recurse tree leaves)
              ; End condition
        (cond ((null? tree) leaves)
              ; CAR is a bare value; put at front of leaves list
              ((not (pair? tree)) (cons tree leaves))
              ; Find the leaves for each side of the tree and join
              ; the lists. This is equivalent to
              ; (append (recurse (car tree) '())
              ;         (recurse (cdr tree) leaves))
              (else (recurse (car tree)
                             (recurse (cdr tree) leaves)))))

    (recurse tree '()))

(define x (list (list 1 2) (list 3 4)))
(newline)
(display (fringe x))
(newline)
(display (fringe (list x x)))
