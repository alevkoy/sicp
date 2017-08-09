; Exercise 2.61
; element-of-set? and intersection-set copied from SICP.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (car set) x)
         (cons (car set)
               (adjoin-set x (cdr set))))
        ((= (car set) x)
         set)
        ; (> (car set) x)
        (else
         (cons x set))))

(define set1 (list 1 2 3 5 7))

(newline)
(display "Adjoin 4 to set1: ")
(display (adjoin-set 4 set1))

(newline)
(display "Adjoin 0 to set1: ")
(display (adjoin-set 0 set1))

(newline)
(display "Adjoin 8 to set1: ")
(display (adjoin-set 8 set1))

(newline)
(display "Adjoin 1 to the empty set: ")
(display (adjoin-set 1 '()))
