; Exercise 2.33
; accumulate and skeletons of map, append, and length adapted from SICP.

(define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op
                          initial
                          (cdr sequence)))))

(define (map p sequence)
    (accumulate (lambda (x y)
                    (cons (p x) y))
                '()
                sequence))

(define x (list 1 2 3 4 5))
(newline)
(display (map square x))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define y (list 6 7 8 9 10))
(newline)
(display (append x y))

(define (length sequence)
    (accumulate (lambda (first length-of-rest)
                    (1+ length-of-rest))
                0
                sequence))

(newline)
(display (length x))

; Exercise 2.34
; Skeleton of horner-eval adapted from SICP.

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                    (+ (* higher-terms x)
                       this-coeff))
                0
                coefficient-sequence))

(define coefficients (list 1 3 0 5 0 1))
(newline)
(display (horner-eval 2 coefficients))

; Exercise 2.35
; Skeleton of count-leaves adapted from SICP.

(define (count-leaves t)
    (accumulate + 0 (map (lambda (subtree)
                             (cond ((null? subtree) 0)
                                   ((not (pair? subtree)) 1)
                                   (else (count-leaves subtree))))
                         t)))

(newline)
(display (count-leaves x))

; Tree with 6 leaves
(define tree (list (list 1 2 3) 4 (list 5 6)))
(newline)
(display (count-leaves tree))

; Exercise 2.36
; Skeleton of accumulate-n adapted from SICP.

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define list4 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(newline)
(display (accumulate-n + 0 list4))
