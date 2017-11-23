; Exercise 2.74
; attach-tag copied from SICP.

(define (attach-tag type-tag contents)
  (cons type-tag contents))

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

; Part 1

(define (division-file file)
  (car file))

(define (contents-file file)
  (cdr file))

(define (get-record file name)
   ((get 'get-record (division-file file))
    (contents-file file)
    name))

(define (install-division-a-get-record-package)
  ; Internal procedures
  (define (name-record record)
    (car record))

  (define (get-record-a contents name)
    (cond ((null? contents) #f)
          ((string=? (name-record (car contents)) name)
           (car contents))
          (else (get-record-a (cdr contents) name))))

  ; External interface
  (put 'get-record 'division-a get-record-a)

  'done)

(define (install-division-b-get-record-package)
  ; Internal procedures
  (define (name-record record)
    (car record))

  (define (head tree)
    (car tree))

  (define (left-branch tree)
    (cadr tree))

  (define (right-branch tree)
    (caddr tree))

  (define (get-record-b contents name)
    (if (null? contents)
        #f
        (let ((head-record (head contents))
              (head-name (name-record (head contents))))
          (cond ((string<? name head-name)
                 (get-record-b (left-branch contents) name))
                ((string>? name head-name)
                 (get-record-b (right-branch contents) name))
                (else head-record)))))

  ; External interface
  (put 'get-record 'division-b get-record-b)

  'done)

(install-division-a-get-record-package)
(install-division-b-get-record-package)

; Division files are pairs in which the first element is a tag identifying the
; division, and the second element is the contents of the file, in a format
; specific to the division owning the file.
; Division A stores employee records as an unordered list. Each record is a list
; with two elements: The employee's name and salary, in that order.
(define division-a-file
  (cons
    'division-a
    (list (list "Lacy" 100000)
          (list "George" 80000))))

; Division B stores employee records as a binary search tree, ordered by
; employee name. Each record is an ordered pair of name and salary, in that
; order.
(define division-b-file
  (cons
    'division-b
    (list (cons "Carl" 50000)
          (list (cons "Alisha" 40000) '() '())
          (list (cons "Pete" 60000) '() '()))))

(newline)
(display "George's record in division A: ")
(display (get-record division-a-file "George"))

(newline)
(display "Bob's record in division A: ")
(display (get-record division-a-file "Bob"))

(newline)
(display "Lacy's record in division A: ")
(display (get-record division-a-file "Lacy"))

(newline)
(display "Alisha's record in division B: ")
(display (get-record division-b-file "Alisha"))

(newline)
(display "Pete's record in division B: ")
(display (get-record division-b-file "Pete"))

(newline)
(display "Jill's record in division B: ")
(display (get-record division-b-file "Jill"))

; Part 2

(define (get-salary file name)
  (let ((record (get-record file name)))
    (if (not record)
        #f
        ((get 'get-salary (division-file file))
         record))))

(define (install-division-a-get-salary-package)
  ; Internal procedures
  (define (get-salary-a record)
    (cadr record))

  ; External interface
  (put 'get-salary 'division-a get-salary-a)

  'done)

(define (install-division-b-get-salary-package)
  ; Internal procedures
  (define (get-salary-b record)
    (cdr record))

  ; External interface
  (put 'get-salary 'division-b get-salary-b)

  'done)

(install-division-a-get-salary-package)
(install-division-b-get-salary-package)

(newline)
(display "George's salary in division A: ")
(display (get-salary division-a-file "George"))

(newline)
(display "Bob's salary in division A: ")
(display (get-salary division-a-file "Bob"))

(newline)
(display "Alisha's salary in division B: ")
(display (get-salary division-b-file "Alisha"))

(newline)
(display "Pete's salary in division B: ")
(display (get-salary division-b-file "Pete"))

; Part 3

(define (find-employee-record files name)
  (if (null? files)
      #f
      (let ((record (get-record (car files) name)))
        (if record
            record
            (find-employee-record (cdr files) name)))))

(define files (list division-a-file division-b-file))

(newline)
(display "George's record: ")
(display (find-employee-record files "George"))

(newline)
(display "Lacy's record: ")
(display (find-employee-record files "Lacy"))

(newline)
(display "Alisha's record: ")
(display (find-employee-record files "Alisha"))

(newline)
(display "Pete's record: ")
(display (find-employee-record files "Pete"))

(newline)
(display "Jill's record: ")
(display (find-employee-record files "Jill"))
