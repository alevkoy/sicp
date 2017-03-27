; Exercise 2.30
; Solutions take the same form as the direct and recursive definitions of
; scale-tree in SICP

; Direct definition
(define (square-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(newline)
(display "Direct definition of square-tree: ")
(display (square-tree t))

; Recursive definition with map
(define (square-tree tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree sub-tree)
                (square sub-tree)))
         tree))

(newline)
(display "Recursive definition of square-tree: ")
(display (square-tree t))

; Exercise 2.31

(define (tree-map op tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map op sub-tree)
                (op sub-tree)))
         tree))

(define (square-tree tree)
    (tree-map square tree))

(newline)
(display "Abstract definition of square-tree: ")
(display (square-tree t))
