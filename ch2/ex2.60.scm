; Exercise 2.60
; element-of-set? and intersection-set copied from SICP.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union-set set1 set2)
  (append set1 set2))


(define set1 (list 2 3 2 1 3 2 2))
(define set2 (list 5 2 5 1 2 4))

(newline)
(display "3 an element?: ")
(display (element-of-set? 3 set1))

(newline)
(display "Adjoin 5 to set1: ")
(display (adjoin-set 5 set1))

(newline)
(display "Intersection of set1 and set2: ")
(display (intersection-set set1 set2))

(newline)
(display "Union of set1 and set2: ")
(display (union-set set1 set2))
