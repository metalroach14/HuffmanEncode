;; Toma Tomov, Huffman encoding


(load "trees.rkt")
(load "helper_functions.rkt")


(define (encode data predicate?)
  (if (null? data) (cons "" empty-tree)
      (let* (
             (frequency-list (histogram predicate? data))
             (initial-set-of-trees (build-leaves (insertion-sort-nodes frequency-list)))
             (huffmann-tree (build-huffmann-tree initial-set-of-trees))
             (table-for-replacement (to-table (map path-to-code (paths huffmann-tree frequency-list))))
             (encoded-data (encode-data huffmann-tree data))
             (result (cons encoded-data huffmann-tree))
             ) 
      
        result)))


(define (decode encoded-string huffmann-tree)
  (define (decode-helper encoded-string tree)
  (if (or (null? encoded-string) (= (string-length encoded-string) 0) (null? tree)) '()
      (if (equal? (string-ref encoded-string 0) #\0)
          (if (is-leaf? (left-tree tree)) (cons (caar (left-tree tree)) (decode-helper (substring encoded-string 1) huffmann-tree))
              (decode-helper (substring encoded-string 1) (left-tree tree)))
          (if (is-leaf? (right-tree tree)) (cons (caar (right-tree tree))  (decode-helper (substring encoded-string 1) huffmann-tree))
              (decode-helper (substring encoded-string 1) (right-tree tree))))))
  (decode-helper encoded-string huffmann-tree))


(load "tests.rkt")
