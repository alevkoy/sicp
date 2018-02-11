; Exercise 2.82

; Begin copied from Stack Overflow
; Question: https://stackoverflow.com/q/5499005
; by KnowsLittle: https://stackoverflow.com/users/441693/knowslittle
; Answer: https://stackoverflow.com/a/19114031
; by maxbublis: https://stackoverflow.com/users/2592125/maxbublis

(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

; End copied from Stack Overflow

; Begin adapted from Eli Bendersky's website

; Eli has the equivalence predicate here as 'equal. This may have been a
; transcription error on Eli's part or a change between MIT Scheme versions.
(define *coercion-table* (make-hash-table equal?))

; hash-table-put! and hash-table-get replaced with hash-table/put! and
; hash-table/get, respectively. Perhaps another transcription error.
(define (put-coercion type-from type-to proc)
  (hash-table/put!
    *coercion-table*
    (list type-from type-to)
    proc))

(define (get-coercion type-from type-to)
  (hash-table/get
    *coercion-table*
    (list type-from type-to)
    #f))

; End adapted from Eli Bendersky's website

; Begin copied from SICP

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1
                                     type2))
                      (t2->t1
                       (get-coercion type2
                                     type1)))
                  (cond (t1->t2
                         (apply-generic
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic
                          op a1 (t2->t1 a2)))
                        (else
                         (error
                          "No method for
                           these types"
                          (list
                           op
                           type-tags))))))
              (error
               "No method for these types"
               (list op type-tags)))))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag
   (contents n) 0))

(put-coercion 'scheme-number 'complex
              scheme-number->complex)

; End copied from SICP

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; Rectangular additions hacked into complex package
(put 'make-from-real-imag 'rectangular
     (lambda (x y) (attach-tag 'rectangular (cons x y))))
(put 'real-part '(rectangular) (lambda (c) (car c)))
(put 'imag-part '(rectangular) (lambda (c) (cdr c)))

; Hacked-in coercion of rational to complex. This won't actually be invoked, due
; to deficiencies in the 2.82 version of apply-generic.
(put-coercion 'rational 'complex
              (lambda (r)
                (let ((n (car (contents r)))
                      (d (cdr (contents r))))
                  (make-complex-from-real-imag (/ n d) 0))))

(define (apply-generic op . args)
  (define (apply-generic-error type-tags)
    (error "No method for these types"
           (list type-tags)))

  ; Get a list of coercion functions from the types in |type-tags| to
  ; the type |target|. Use the identity function when the types are
  ; the same.
  (define (get-coercions target type-tags)
    (define (get-coercion-or-identity from-type)
      (if (equal? from-type target)
          (lambda (x) x)
          (get-coercion from-type target)))

    (if (null? type-tags)
        (error "Can't coerce empty set of types to target" target)
        (map get-coercion-or-identity type-tags)))

  ; Try to coerce the arguments into the types of successive arguments
  ; until a working set of coercions is found, or the possiblities are
  ; exhausted.
  (define (apply-coercion type-tags target-tags)
    (if (null? target-tags)
        (apply-generic-error type-tags)
        (let ((coercions (get-coercions (car target-tags)
                                        type-tags)))
          (if (any not coercions)
              (apply-coercion type-tags (cdr target-tags))
              (apply apply-generic
                     op
                     (map (lambda (coercion arg) (coercion arg))
                          coercions args))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-coercion type-tags type-tags)))))

; Test cases
; 1. Args of same type, operation defined.
; 2. Args of different types, one complex. Operation defined for complex.
; 3. Args of type scheme-number and rational. Operation defined for complex.
;    (Won't work.)

(define (add3 x y z)
  (apply-generic 'add x y z))
(define (real-part c)
  (apply-generic 'real-part c))
(define (imag-part c)
  (apply-generic 'imag-part c))

(put 'add '(complex complex complex)
     (lambda (x y z)
       (make-complex-from-real-imag
         (+ (real-part x) (real-part y) (real-part z))
         (+ (imag-part x) (imag-part y) (imag-part z)))))

(newline)
(display "1+2i + 2+4i + 3+8i: ")
(display (add3 (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 2 4)
               (make-complex-from-real-imag 3 8)))

(newline)
(display "1 + 2 + 3+8i: ")
(display (add3 (make-scheme-number 1)
               (make-scheme-number 2)
               (make-complex-from-real-imag 3 8)))

; This won't work with the 2.82 version of apply-generic, because scheme-number
; is not coerceable to rational, even though both are coerceable to complex.
; scheme-number should actually be coerceable to rational as well, but I left
; that conversion undefined to demonstrate a general limitation of the
; algorithm.
;(newline)
;(display "1 + 2 + 5/6: ")
;(display (add3 (make-scheme-number 1)
;               (make-scheme-number 2)
;               (make-rational 5 6)))
