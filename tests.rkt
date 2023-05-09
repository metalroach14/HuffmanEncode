;;tests for Huffman Encoding

;; test with string presented as a list of chars
(define test-data1 (string->list "abracadabra"))
(define result (encode test-data1 equal?))
(define test-data1-encoded (car result))
(define test-data1-tree (cdr result))
(define decoded-test-data1 (decode test-data1-encoded test-data1-tree))
(equal? test-data1 decoded-test-data1)

;; simple numeric list test
(define test-data2 '(5 6 3 9 9 7 7 11 1 1))
(define result2 (encode test-data2 equal?))
(define test-data2-encoded (car result2))
(define test-data2-tree (cdr result2))
(define decoded-test-data2 (decode test-data2-encoded test-data2-tree))
(equal? test-data2 decoded-test-data2)

;; list of functions 
(define test-data3 '(odd? even? even? char? null?))
(define result3 (encode test-data3 equal?))
(define test-data3-encoded (car result3))
(define test-data3-tree (cdr result3))
(define decoded-test-data3 (decode test-data3-encoded test-data3-tree))
(equal? test-data3 decoded-test-data3)

;; polymorphic list example
(define complex-example '(423 423 #t #t #t #t (1 2 3 4) #f #f "abv" (1 2 3 4) ( (1 2 3) (1 2 3)) #t #t 423 #f "abv"))
(define complex-result (encode complex-example equal?))
(define complex-encoded (car complex-result))
(define complex-tree (cdr complex-result))
(define decoded-complex-example (decode complex-encoded complex-tree))
(equal? complex-example decoded-complex-example)
