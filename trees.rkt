;;; trees

(define empty-tree '())
(define empty-tree? null?)

(define (make-tree root left right)
  (list root left right))

(define (make-leaf element)
  (make-tree element empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define (tree? tree)
  (or
   (null? tree)
   (and
    (list? tree)
    (= (length tree) 3)
    (tree? (left-tree tree))
    (tree? (right-tree tree)))))

(define (is-leaf? leaf)
  (and
   (not (null? (root-tree leaf)))
   (null? (left-tree leaf))
   (null? (right-tree leaf))))

(define (is-element-of-tree-improved? x tree)
  (if (null? tree) #f
      (if (is-leaf? tree) (equal? x (caar tree))
          (if (equal? x (root-tree tree)) #t
              (or (is-element-of-tree-improved? x (left-tree tree)) (is-element-of-tree-improved? x (right-tree tree)))))))

(define (path-improved element tree)
      (if (or (not (is-element-of-tree-improved? element tree)) (is-leaf? tree)) '()
          (if (is-element-of-tree-improved? element (left-tree tree))
              (cons "0" (path-improved element (left-tree tree)))
              (cons "1" (path-improved element (right-tree tree))))))

(define (is-element-of-tree? x tree)
  (if (null? tree) #f
      (if (equal? x (root-tree tree)) #t
          (or (is-element-of-tree? x (left-tree tree)) (is-element-of-tree? x (right-tree tree))))))

(define (path element tree)
  (if (not (is-element-of-tree? element tree)) #f
      (if (equal? (root-tree tree) element) (list element)
          (if (is-element-of-tree? element (left-tree tree))
              (cons "0" (path element (left-tree tree)))
              (cons "1" (path element (right-tree tree)))))))

(define (paths tree elems)
  (define (helper elems)
    (if (null? elems) '()
        (cons (path (car elems) tree) (helper (cdr elems)))))
  (helper elems))

;;; end trees