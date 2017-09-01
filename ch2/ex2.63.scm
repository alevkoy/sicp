; Exercise 2.63
; entry, left-branch, right-branch, make-tree, element-of-set?, adjoin-set,
; tree->list-1, and tree->list-2 copied from SICP.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
        (tree->list-1
          (left-branch tree))
        (cons (entry tree)
              (tree->list-1
                (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
          (left-branch tree)
          (cons (entry tree)
                (copy-to-list
                  (right-branch tree)
                  result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    (make-tree 5 '() '()))
                         (make-tree 9
                                    '()
                                    (make-tree 11 '() '()))))

(define tree2 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))

(newline)
(display "Routine 1:")
(newline)
(display "Tree 1: ")
(display (tree->list-1 tree1))
(newline)
(display "Tree 2: ")
(display (tree->list-1 tree2))
(newline)
(display "Tree 3: ")
(display (tree->list-1 tree3))
(newline)
(display "Routine 2:")
(newline)
(display "Tree 1: ")
(display (tree->list-2 tree1))
(newline)
(display "Tree 2: ")
(display (tree->list-2 tree2))
(newline)
(display "Tree 3: ")
(display (tree->list-2 tree3))
; Both routines produce (1 3 5 7 9) for all 3 trees.

; Exercise 2.64
; list->tree and partial-tree copied from SICP.

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(newline)
(display "(list->tree '(1 3 5 7 9 11)): ")
(display (list->tree '(1 3 5 7 9 11)))

; Exercise 2.65

(define (union-set set1 set2)
  (define (union-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
            (let ((x1 (car set1))
                  (x2 (car set2)))
              (cond ((= x1 x2)
                     (cons x1
                           (union-list (cdr set1) (cdr set2))))
                    ((< x1 x2)
                     (cons x1
                           (union-list (cdr set1) set2)))
                    ((< x2 x1)
                     (cons x2
                           (union-list set1 (cdr set2)))))))))

  (let ((set1-list (tree->list-2 set1))
        (set2-list (tree->list-2 set2)))
    (list->tree (union-list set1-list set2-list))))

(define set1 '(5
               (2
                 (1 () ())
                 (3 () ()))
               (9
                   (8
                       (7 () ())
                       ())
                   ())))

(define set2 '(1
               ()
               (4
                 ()
                 (6
                   ()
                   (8
                     ()
                     (10 () ()))))))

(newline)
(display "Union of set1 and set2: ")
(display (union-set set1 set2))

(define (intersection-set set1 set2)
  (define (intersection-list set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-list
                            (cdr set1)
                            (cdr set2))))
                ((< x1 x2) (intersection-list
                             (cdr set1)
                             set2))
                ((< x2 x1) (intersection-list
                             set1
                             (cdr set2)))))))

  (let ((set1-list (tree->list-2 set1))
        (set2-list (tree->list-2 set2)))
    (list->tree (intersection-list set1-list set2-list))))

(newline)
(display "Intersection of set1 and set2: ")
(display (intersection-set set1 set2))
