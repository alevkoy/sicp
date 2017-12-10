; Exercise 2.78

; Begin copied from SICP

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types:
             APPLY-GENERIC"
            (list op type-tags))))))

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

; End copied from SICP

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

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(newline)
(display "Scheme numbers:")

(newline)
(display "4 - 3 = ")
(display (sub (make-scheme-number 4) (make-scheme-number 3)))

(newline)
(display "7 + 2 = ")
(display (add (make-scheme-number 7) (make-scheme-number 2)))

(newline)
(display "3 * 5 = ")
(display (mul (make-scheme-number 3) (make-scheme-number 5)))

(newline)
(display "20 / 4 = ")
(display (div (make-scheme-number 20) (make-scheme-number 4)))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum:
                     TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum:
                     CONTENTS" datum))))

(newline)
(display "Plain numbers:")

; The numbers below could equivalently be specified using make-scheme-number,
; since make-scheme-number doesn't do anything anymore.
(newline)
(display "4 - 3 = ")
(display (sub 4 3))

(newline)
(display "7 + 2 = ")
(display (add 7 2))

(newline)
(display "3 * 5 = ")
(display (mul 3 5))

(newline)
(display "20 / 4 = ")
(display (div 20 4))

; Exercise 2.79

; A few of the complex-number primitives were not converted to the data-directed
; style in SICP. Here, I have hacked in the ones that are needed for my tests,
; but they really should be in the complex arithmetic package copied from SICP
; above.
(put 'make-from-real-imag 'rectangular
     (lambda (x y) (attach-tag 'rectangular (cons x y))))
(put 'real-part '(rectangular) (lambda (c) (car c)))
(put 'imag-part '(rectangular) (lambda (c) (cdr c)))

(define (equ? a b)
  (apply-generic 'equ? a b))

(define (install-equ?-package)
  ; Internal procedures
  (define (equ?-scheme-number a b)
    ; The result is a boolean, so it doesn't get a type tag.
    (= a b))

  ; numer and denom copied from SICP.
  ; It would make more sense to define these implementations in the
  ; packages for their respective types, but I want to keep the
  ; solutions for exercises 2.78 and 2.79 separate.
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (equ?-rational a b)
    (and (= (numer a) (numer b))
         (= (denom a) (denom b))))

  (define (real-part c)
    (apply-generic 'real-part c))
  (define (imag-part c)
    (apply-generic 'imag-part c))
  (define (equ?-complex a b)
    (and (= (real-part a) (real-part b))
         (= (imag-part a) (imag-part b))))

  ; External interface
  (put 'equ? '(scheme-number scheme-number) equ?-scheme-number)
  (put 'equ? '(rational rational) equ?-rational)
  (put 'equ? '(complex complex) equ?-complex)

  'done)

(install-equ?-package)

(newline)
(display "3 == 3? ")
(display (equ? (make-scheme-number 3) (make-scheme-number 3)))

(newline)
(display "-1 == 9? ")
(display (equ? (make-scheme-number -1) (make-scheme-number 9)))

(newline)
(display "2/3 == 6/9? ")
(display (equ? (make-rational 2 3) (make-rational 6 9)))

(newline)
(display "5/4 == 24/20? ")
(display (equ? (make-rational 5 4) (make-rational 24 20)))

(newline)
(display "4 + 5i == 4 + 5i? ")
(display (equ? (make-complex-from-real-imag 4 5)
               (make-complex-from-real-imag 4 5)))

(newline)
(display "4 + 5i == 3 + 7i? ")
(display (equ? (make-complex-from-real-imag 4 5)
               (make-complex-from-real-imag 3 7)))

; Exercise 2.80

(define (=zero? x)
  (let ((zero ((get 'make-zero (type-tag x)))))
    (equ? x zero)))

(define (install-=zero?-package)
  ; External interface
  (put 'make-zero 'scheme-number (lambda () (make-scheme-number 0)))
  (put 'make-zero 'rational (lambda () (make-rational 0 1)))
  (put 'make-zero 'complex (lambda () (make-complex-from-real-imag 0 0)))

  'done)

(install-=zero?-package)

(newline)
(display "0 == 0? ")
(display (=zero? (make-scheme-number 0)))

(newline)
(display "1 == 0? ")
(display (=zero? (make-scheme-number 1)))

(newline)
(display "0/2 == 0? ")
(display (=zero? (make-rational 0 1)))

(newline)
(display "1/2 == 0? ")
(display (=zero? (make-rational 1 2)))

(newline)
(display "0+0i == 0? ")
(display (=zero? (make-complex-from-real-imag 0 0)))

(newline)
(display "0+1i == 0? ")
(display (=zero? (make-complex-from-real-imag 0 1)))

(newline)
(display "1+0i == 0? ")
(display (=zero? (make-complex-from-real-imag 1 0)))
