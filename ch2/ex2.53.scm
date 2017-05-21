; Exercise 2.53
; memq adapted from SICP.

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

; The actual code for the solution was written interactively.
