; Exercise 1.11

; Recursive process
(define (f-rec n)
 (if (< n 3)
     n
     (+ (f-rec (- n 1))
        (* 2 (f-rec (- n 2)))
        (* 3 (f-rec (- n 3))))))

; Iterative process
(define (f-it n)
 ; Perform nontrival f(n) computation
 (define (reduce f-1 f-2 f-3)
  (+ f-1 (* 2 f-2) (* 3 f-3)))
 ; Evaluate successive f(n)s starting at 3 until we reach n
 (define (iterate i f-1 f-2 f-3)
  (cond ((= i (+ n 1)) f-1)
        (else (iterate (+ i 1)
                       (reduce f-1 f-2 f-3)
                       f-1
                       f-2))))
 (if (< n 3)
     n
     (iterate 3 2 1 0)))
