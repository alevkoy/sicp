; Exercise 2.66
; entry, left-branch, and right-branch copied from SICP.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (key record)
  (car record))

(define (value record)
  (cadr record))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((entry-record (entry set-of-records))
            (entry-key (key (entry set-of-records))))
        (cond ((= given-key entry-key)
               entry-record)
              ((< given-key entry-key)
               (lookup given-key (left-branch set-of-records)))
              ((> given-key entry-key)
               (lookup given-key (right-branch set-of-records)))))))

(define tree
  '((5 "five")
    ((3 "three")
     ((2 "two") () ())
     ((4 "four") () ()))
    ((6 "six") () ())))

(newline)
(display "Value of record 4: ")
(display (value (lookup 4 tree)))

(newline)
(display "Value of record 6: ")
(display (value (lookup 6 tree)))

(newline)
(display "Record 7?: ")
(display (lookup 7 tree))
