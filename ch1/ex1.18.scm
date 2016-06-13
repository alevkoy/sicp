(define (halve x) (/ x 2))
(define (double x) (+ x x))

; Exercise 1.18
; This exercise is very similar to 1.17, and this code is actually what I
; originally produced for 1.17. I suppose that 1.17's requirement that the
; solution be "analogous to fast-expt" means that 1.17 must generate a recursive
; process, but that was not immediately obvious to me, and (I suppose) a
; constant-space iterative process is better than a linear-space recursive
; process.
(define (mult x y)
 ; Iteration invariant: a+x*y
 (define (mult-iter x y a)
  ; Algorithm doesn't work with negative y, because decrementing
  ; needs to take y closer to 0. Handle that by negating both x
  ; and y if y is negative. This won't change the product, but
  ; it will make y nonnegative (regardless of x's sign before or
  ; after the change).
  (cond ((< y 0) (mult-iter (- x) (- y) a))
        ((= y 0) a)
        ((even? y) (mult-iter (double x) (halve y) a))
        (else (mult-iter x (-1+ y) (+ a x)))))
 (mult-iter x y 0))
