#lang racket

;; Tests for basic rel & tuple construction/manipulation, plus print-rel.

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide core-suite)

;; ---------------------------------------------------------------------------
;; Sample data
;; ---------------------------------------------------------------------------

(define employees-desc (tuple-desc '(id name dept-id)))
(define employees
  (rel employees-desc
       (list (tuple 1 "Alice"   10)
             (tuple 2 "Bob"     20)
             (tuple 3 "Carol"   10)
             (tuple 4 "Dan"     '())))) ;; null dept-id

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(define tuple-tests
  (test-suite
   "Tuple constructor"
   (test-case "values are stored in order"
     (check-equal? (tuple-values (tuple 1 2 3)) '(1 2 3)))
   (test-case "tuple? recognises tuples"
     (check-true  (tuple? (tuple 1)))
     (check-false (tuple? (list 1 2 3)))
     (check-false (tuple? "not a tuple")))
   (test-case "empty tuple is allowed"
     (check-equal? (tuple-values (tuple)) '()))
   (test-case "null values stored as '()"
     (check-equal? (tuple-values (tuple 1 '() 3)) '(1 () 3)))
   (test-case "tuples with equal values are equal?"
     (check-equal? (tuple 1 "x" '()) (tuple 1 "x" '())))
   (test-case "tuple-ref looks up by field name"
     (check-equal? (tuple-ref (tuple 1 "Alice" 10) employees-desc 'name)
                   "Alice")
     (check-equal? (tuple-ref (tuple 1 "Alice" 10) employees-desc 'dept-id)
                   10))))

(define rel-tests
  (test-suite
   "Rel constructor"
   (test-case "rel stores its desc and tuples"
     (check-equal? (rel-desc employees) employees-desc)
     (check-equal? (length (rel-tuples employees)) 4))
   (test-case "rel allows duplicate tuples"
     (define r (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 2))))
     (check-equal? (length (rel-tuples r)) 3))))

(define print-rel-tests
  (test-suite
   "print-rel"
   (test-case "renders headers, padding, and NULL for '()"
     (define small
       (rel (tuple-desc '(id name))
            (list (tuple 1 "Alice")
                  (tuple 22 '()))))
     (define out (open-output-string))
     (print-rel small out)
     (check-equal? (get-output-string out)
                   (string-append
                    "+----+-------+\n"
                    "| id | name  |\n"
                    "+----+-------+\n"
                    "| 1  | Alice |\n"
                    "| 22 | NULL  |\n"
                    "+----+-------+\n")))
   (test-case "empty rel still prints headers"
     (define empty-r (rel (tuple-desc '(a bb)) '()))
     (define out (open-output-string))
     (print-rel empty-r out)
     (check-equal? (get-output-string out)
                   (string-append
                    "+---+----+\n"
                    "| a | bb |\n"
                    "+---+----+\n"
                    "+---+----+\n")))))

(define core-suite
  (test-suite
   "core"
   tuple-tests
   rel-tests
   print-rel-tests))

(module+ main
  (exit (if (zero? (run-tests core-suite)) 0 1)))

(module+ test
  (run-tests core-suite))
