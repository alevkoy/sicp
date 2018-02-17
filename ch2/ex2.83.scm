; Exercise 2.83

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

; Begin adapted from SICP

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

  ; Hacked-in support for public acess to numer and denom
  ; XXX: This will produce untyped results, which will not be suitable for use
  ; in generic operations.
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)

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

; Hacked-in support for imaginary.
(put 'make 'imaginary (lambda (x)
                        (attach-tag 'imaginary x)))
(put-coercion 'imaginary 'complex
              (lambda (i)
                (make-complex-from-real-imag 0 (contents i))))
(define (make-imaginary x)
  ((get 'make 'imaginary) x))

; Hacked in support for rational accessors
(define (numer-rational q)
  ((get 'numer 'rational) q))

(define (denom-rational q)
  ((get 'denom 'rational) q))

; Hacked in support for real
(define (make-real x)
  (attach-tag 'real x))

; Coercions suitable for raise
(put-coercion 'scheme-number 'rational
              (lambda (x)
                (make-rational (contents x)
                               1)))

(put-coercion 'rational 'real
              (lambda (x)
                (make-real (/ (numer-rational (contents x))
                              (denom-rational (contents x))))))

(put-coercion 'real 'complex
              (lambda (x)
                (make-complex-from-real-imag (contents x)
                                             0)))

(define (raise n)
  (define (get-result-type tag)
    (cond
      ((equal? tag 'scheme-number) 'rational)
      ((equal? tag 'rational) 'real)
      ((equal? tag 'real) 'complex)
      (else (error "Cannot raise type " tag))))

  (let ((from-type (type-tag n))
        (to-type (get-result-type (type-tag n))))
    ((get-coercion from-type to-type) n)))

(define n (make-scheme-number 3))
(newline)
(display "scheme-number: ")
(display n)

(define q (raise n))
(newline)
(display "rational: ")
(display q)

(define r (raise q))
(newline)
(display "real: ")
(display r)

(define c (raise r))
(newline)
(display "complex: ")
(display c)
