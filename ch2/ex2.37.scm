; Exercise 2.37
; accumulate, dot-product, and skeletons for matrix-*-vector, transpose, and
; matrix-*-matrix adapted from SICP. accumulate-n copied from Exercise 2.36.

(define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op
                          initial
                          (cdr sequence)))))

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

; v is taken to be a column vector with the same number of rows as m has columns
(define (matrix-*-vector m v)
    (map (lambda (row)
             (dot-product row v))
         m))

(define m3x3 (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)))
(define v3 (list 1 2 3))

(newline)
(display "Matrix-vector product:")
(newline)
(display (matrix-*-vector m3x3 v3))

(define (transpose mat)
    (accumulate-n cons '() mat))

(newline)
(display "Transpose:")
(newline)
(display (transpose m3x3))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row)
                 (map (lambda (col)
                          (dot-product row col))
                      cols))
             m)))

(define m3x2 (list (list 1 2)
                   (list 3 4)
                   (list 5 6)))

(newline)
(display "Matrix-matrix product:")
(newline)
(display (matrix-*-matrix m3x3 m3x2))
