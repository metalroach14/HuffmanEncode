
;;;helper functions 

(define (count-occurances predicate? x l)
  (if (null? l) 0
      (if (predicate? x (car l))
          (+ 1 (count-occurances predicate? x (cdr l)))
          (+ 0 (count-occurances predicate? x (cdr l))))))

(define (remove predicate? x l)
  (if (null? l) '()
      (if (predicate? x (car l))
          (remove predicate? x (cdr l))
          (cons (car l) (remove predicate? x (cdr l))))))

(define (histogram predicate? lst)
  (if (null? lst) '()
      (cons (cons (car lst) (count-occurances predicate? (car lst) lst)) (histogram predicate? (remove predicate? (car lst) lst)))))

(define (cmp-nodes? n1 n2)
  (< (cdr n1) (cdr n2)))

(define (insert-in-sorted x lst)
  (if (null? lst) (list x)
      (if (< x (car lst)) (append (list x) lst)
          (cons (car lst) (insert-in-sorted x (cdr lst))))))

(define (insert-in-sorted-nodes x lst)
  (if (null? lst) (list x)
      (if (cmp-nodes? x (car lst)) (append (list x) lst)
          (cons (car lst) (insert-in-sorted-nodes x (cdr lst))))))

(define (insertion-sort-nodes lst)
  (if (null? lst) '()
      (insert-in-sorted-nodes (car lst) (insertion-sort-nodes (cdr lst)))))

(define (build-leaves frequency-list)
  (if (null? frequency-list) '()
      (cons (make-leaf (car frequency-list)) (build-leaves (cdr frequency-list)))))

(define (insert-in-sorted-trees new-tree set-of-trees)
  (cond ((null? set-of-trees) (list new-tree))
        ((and
          (is-leaf? new-tree)
          (is-leaf? (car set-of-trees)))
         (if (< (cdr (root-tree new-tree)) (cdr (root-tree (car set-of-trees))))
             (append (list new-tree) set-of-trees)
             (cons (car set-of-trees) (insert-in-sorted-trees new-tree (cdr set-of-trees)))))

        ((and
          (is-leaf? new-tree)
          (not (is-leaf? (car set-of-trees))))
         (if (< (cdr (root-tree new-tree)) (root-tree (car set-of-trees)))
             (append (list new-tree) set-of-trees)
             (cons (car set-of-trees) (insert-in-sorted-trees new-tree (cdr set-of-trees)))))

        ((and
          (not (is-leaf? new-tree))
          (is-leaf? (car set-of-trees)))
         (if (< (root-tree new-tree) (cdr (root-tree (car set-of-trees))))
             (append (list new-tree) set-of-trees)
             (cons (car set-of-trees) (insert-in-sorted-trees new-tree (cdr set-of-trees)))))

        (else  (if (< (root-tree new-tree) (root-tree (car set-of-trees)))
                   (append (list new-tree) set-of-trees)
                   (cons (car set-of-trees) (insert-in-sorted-trees new-tree (cdr set-of-trees)))))))

 
(define (build-huffmann-tree set-of-trees)
  (if (< (length set-of-trees) 2) (car set-of-trees)
      (build-huffmann-tree (cddr (insert-in-sorted-trees (merge-trees (car set-of-trees) (cadr set-of-trees)) set-of-trees)))))

      
(define (merge-trees tree1 tree2)
  (cond ((and
          (is-leaf? tree1)
          (is-leaf? tree2))
         (make-tree (+ (cdr (root-tree tree1)) (cdr (root-tree tree2))) tree1 tree2))
        ((and
          (is-leaf? tree1)
          (not (is-leaf? tree2)))
         (make-tree (+ (cdr (root-tree tree1)) (root-tree tree2)) tree1 tree2))
        ((and
          (not (is-leaf? tree1))
          (is-leaf? tree2))
         (make-tree (+ (root-tree tree1) (cdr (root-tree tree2))) tree1 tree2))
        (else
         (make-tree (+ (root-tree tree1) (root-tree tree2)) tree1 tree2))))

(define (pre-encode tree data)
  (if (null? data) '()
      (cons (path-improved (car data) tree) (pre-encode tree (cdr data)))))

(define (encode-data tree data)
  (apply string-append (map (lambda (lst) (apply string-append lst))(pre-encode tree data))))


(define (path-to-code lst)
  (cons (apply string-append (reverse (cdr (reverse lst)))) (car (reverse lst))))

(define (to-table table)
  (map (lambda (lst) (cons (car lst) (cadr lst))) table))


;; end helper functions