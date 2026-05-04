#lang racket

(require rackunit
         rackunit/text-ui
         "relsim.rkt")

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

(define depts-desc (tuple-desc '(dept-id dept-name)))
(define depts
  (rel depts-desc
       (list (tuple 10 "Eng")
             (tuple 20 "Sales")
             (tuple 30 "HR"))))

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

(define select-tests
  (test-suite
   "Select"
   (test-case "filters rows by predicate"
     (define r (select (lambda (t)
                         (equal? (tuple-ref t employees-desc 'dept-id) 10))
                       employees))
     (check-equal? (length (rel-tuples r)) 2)
     (check-equal? (rel-desc r) employees-desc))
   (test-case "select with always-false yields empty rel"
     (define r (select (lambda (_) #f) employees))
     (check-equal? (rel-tuples r) '()))
   (test-case "select preserves duplicates"
     (define dup (rel (tuple-desc '(x))
                      (list (tuple 1) (tuple 1) (tuple 2))))
     (define r (select (lambda (t) (equal? (tuple-ref t (rel-desc dup) 'x) 1))
                       dup))
     (check-equal? (length (rel-tuples r)) 2))))

(define project-tests
  (test-suite
   "Project"
   (test-case "keeps only requested fields"
     (define r (project '(name) employees))
     (check-equal? (tuple-desc-fields (rel-desc r)) '(name))
     (check-equal? (map tuple-values (rel-tuples r))
                   '(("Alice") ("Bob") ("Carol") ("Dan"))))
   (test-case "can reorder fields"
     (define r (project '(name id) employees))
     (check-equal? (tuple-desc-fields (rel-desc r)) '(name id))
     (check-equal? (tuple-values (car (rel-tuples r))) '("Alice" 1)))
   (test-case "errors on unknown field"
     (check-exn exn:fail? (lambda () (project '(nope) employees))))))

(define cp-tests
  (test-suite
   "CartesianProduct"
   (test-case "row count is product of inputs"
     (define r (cartesian-product employees depts))
     (check-equal? (length (rel-tuples r)) (* 4 3)))
   (test-case "desc concatenates fields"
     (define r (cartesian-product employees depts))
     (check-equal? (tuple-desc-fields (rel-desc r))
                   '(id name dept-id dept-id dept-name)))
   (test-case "empty rel on either side gives empty product"
     (define empty-r (rel employees-desc '()))
     (check-equal? (rel-tuples (cartesian-product empty-r depts)) '())
     (check-equal? (rel-tuples (cartesian-product depts empty-r)) '()))))

(define join-tests
  (test-suite
   "Join"
   (test-case "equi-join matches on dept-id"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define r (join (lambda (e d)
                       (equal? (tuple-ref e ed 'dept-id)
                               (tuple-ref d dd 'dept-id)))
                     employees depts))
     ;; Alice+Eng, Bob+Sales, Carol+Eng -> 3 rows. Dan (null) and HR drop out.
     (check-equal? (length (rel-tuples r)) 3))
   (test-case "join with always-true is cartesian product"
     (define r (join (lambda (_ __) #t) employees depts))
     (check-equal? (length (rel-tuples r)) 12))
   (test-case "join desc concatenates"
     (define r (join (lambda (_ __) #t) employees depts))
     (check-equal? (tuple-desc-fields (rel-desc r))
                   '(id name dept-id dept-id dept-name)))))

(define semijoin-tests
  (test-suite
   "Semijoin"
   (test-case "keeps left rows that have any match, drops the rest"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define r (semijoin (lambda (e d)
                           (equal? (tuple-ref e ed 'dept-id)
                                   (tuple-ref d dd 'dept-id)))
                         employees depts))
     ;; Alice, Bob, Carol have matching depts; Dan (null dept-id) does not.
     (check-equal? (length (rel-tuples r)) 3)
     (check-equal? (rel-desc r) employees-desc)
     (check-equal? (map (lambda (t) (tuple-ref t employees-desc 'name))
                        (rel-tuples r))
                   '("Alice" "Bob" "Carol")))
   (test-case "preserves duplicate left rows that match"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(y)) (list (tuple 1))))
     (define r (semijoin (lambda (l r)
                           (equal? (tuple-ref l (rel-desc a) 'x)
                                   (tuple-ref r (rel-desc b) 'y)))
                         a b))
     (check-equal? (length (rel-tuples r)) 2))))

(define antijoin-tests
  (test-suite
   "Antijoin"
   (test-case "keeps left rows that have no match"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define r (antijoin (lambda (e d)
                           (equal? (tuple-ref e ed 'dept-id)
                                   (tuple-ref d dd 'dept-id)))
                         employees depts))
     ;; Only Dan (null dept-id) has no match.
     (check-equal? (length (rel-tuples r)) 1)
     (check-equal? (tuple-ref (car (rel-tuples r)) employees-desc 'name)
                   "Dan"))
   (test-case "antijoin against empty rel returns all left rows"
     (define empty-d (rel depts-desc '()))
     (define r (antijoin (lambda (_ __) #t) employees empty-d))
     (check-equal? (length (rel-tuples r)) 4))))

(define union-tests
  (test-suite
   "Union"
   (test-case "appends rows of two rels with same desc"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(x))
                    (list (tuple 2) (tuple 3))))
     (define u (union a b))
     ;; SQL-style: duplicates preserved.
     (check-equal? (length (rel-tuples u)) 4)
     (check-equal? (map tuple-values (rel-tuples u))
                   '((1) (2) (2) (3))))
   (test-case "errors when descs differ"
     (define a (rel (tuple-desc '(x)) (list (tuple 1))))
     (define b (rel (tuple-desc '(y)) (list (tuple 1))))
     (check-exn exn:fail? (lambda () (union a b))))))

(define intersect-tests
  (test-suite
   "Intersect"
   (test-case "multiset intersection: min(m,n) of each tuple"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 3))))
     (define r (intersect a b))
     ;; (1) appears 3 times in a, 2 in b -> 2; (2) absent from b -> 0.
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1) (1))))
   (test-case "preserves order from left rel"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 2) (tuple 3))))
     (define b (rel (tuple-desc '(x))
                    (list (tuple 3) (tuple 2) (tuple 1))))
     (define r (intersect a b))
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1) (2) (3))))
   (test-case "errors on mismatched descs"
     (check-exn exn:fail?
                (lambda ()
                  (intersect (rel (tuple-desc '(x)) '())
                             (rel (tuple-desc '(y)) '())))))))

(define except-tests
  (test-suite
   "Except"
   (test-case "multiset difference: max(m-n, 0) of each tuple"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 3))))
     (define r (except a b))
     ;; (1) 3-1=2; (2) 1-0=1.
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1) (1) (2))))
   (test-case "except with empty right is identity"
     (define a (rel (tuple-desc '(x)) (list (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(x)) '()))
     (check-equal? (rel-tuples (except a b)) (rel-tuples a)))
   (test-case "errors on mismatched descs"
     (check-exn exn:fail?
                (lambda ()
                  (except (rel (tuple-desc '(x)) '())
                          (rel (tuple-desc '(y)) '())))))))

(define outer-join-tests
  (test-suite
   "OuterJoin"
   (test-case "full outer keeps unmatched on both sides with nulls"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define pred (lambda (e d)
                    (equal? (tuple-ref e ed 'dept-id)
                            (tuple-ref d dd 'dept-id))))
     (define r (outer-join pred employees depts #:side 'full))
     ;; 3 matches + Dan (left-only) + HR (right-only) = 5 rows
     (check-equal? (length (rel-tuples r)) 5)
     (define rows (map tuple-values (rel-tuples r)))
     (check-not-false (member '(4 "Dan" () () ()) rows))
     (check-not-false (member '(() () () 30 "HR") rows)))
   (test-case "left outer drops unmatched right"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define pred (lambda (e d)
                    (equal? (tuple-ref e ed 'dept-id)
                            (tuple-ref d dd 'dept-id))))
     (define r (outer-join pred employees depts #:side 'left))
     ;; 3 matches + Dan = 4
     (check-equal? (length (rel-tuples r)) 4))
   (test-case "right outer drops unmatched left"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define pred (lambda (e d)
                    (equal? (tuple-ref e ed 'dept-id)
                            (tuple-ref d dd 'dept-id))))
     (define r (outer-join pred employees depts #:side 'right))
     ;; 3 matches + HR = 4
     (check-equal? (length (rel-tuples r)) 4))
   (test-case "no matches: full outer returns left-padded + right-padded"
     (define a (rel (tuple-desc '(x)) (list (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(y)) (list (tuple 9))))
     (define r (outer-join (lambda (_ __) #f) a b #:side 'full))
     (check-equal? (length (rel-tuples r)) 3)
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1 ()) (2 ()) (() 9))))))

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

(define range-overlaps-tests
  (test-suite
   "range-overlaps"
   (test-case "ranges that share interior points overlap"
     (check-true  (range-overlaps '(1 . 5) '(3 . 7)))
     (check-true  (range-overlaps '(3 . 7) '(1 . 5)))
     (check-true  (range-overlaps '(1 . 10) '(3 . 5)))   ;; one contains the other
     (check-true  (range-overlaps '(3 . 5) '(1 . 10))))
   (test-case "ranges that only touch at an endpoint do not overlap"
     (check-false (range-overlaps '(1 . 3) '(3 . 5)))
     (check-false (range-overlaps '(3 . 5) '(1 . 3))))
   (test-case "fully disjoint ranges do not overlap"
     (check-false (range-overlaps '(1 . 2) '(5 . 9)))
     (check-false (range-overlaps '(5 . 9) '(1 . 2))))
   (test-case "works with non-integer comparable values"
     (check-true  (range-overlaps '(1.5 . 3.5) '(2.0 . 4.0)))
     (check-false (range-overlaps '(1.5 . 2.0) '(2.0 . 3.0))))))

(define range-intersection-tests
  (test-suite
   "range-intersection"
   (test-case "partial overlap returns the shared sub-range"
     (check-equal? (range-intersection '(1 . 5) '(3 . 7)) '(3 . 5))
     (check-equal? (range-intersection '(3 . 7) '(1 . 5)) '(3 . 5)))
   (test-case "containment returns the inner range"
     (check-equal? (range-intersection '(1 . 10) '(3 . 5)) '(3 . 5))
     (check-equal? (range-intersection '(3 . 5) '(1 . 10)) '(3 . 5)))
   (test-case "identical ranges intersect to themselves"
     (check-equal? (range-intersection '(2 . 8) '(2 . 8)) '(2 . 8)))
   (test-case "endpoint-touching ranges return #f"
     (check-false (range-intersection '(1 . 3) '(3 . 5)))
     (check-false (range-intersection '(3 . 5) '(1 . 3))))
   (test-case "fully disjoint ranges return #f"
     (check-false (range-intersection '(1 . 2) '(5 . 9)))
     (check-false (range-intersection '(5 . 9) '(1 . 2))))
   (test-case "agrees with range-overlaps on truthiness"
     (define cases
       '(((1 . 5) (3 . 7))
         ((1 . 3) (3 . 5))
         ((1 . 2) (5 . 9))
         ((1 . 10) (3 . 5))))
     (for ([c (in-list cases)])
       (check-equal? (and (range-intersection (car c) (cadr c)) #t)
                     (range-overlaps (car c) (cadr c)))))))

(define all-tests
  (test-suite
   "relsim"
   tuple-tests
   rel-tests
   select-tests
   project-tests
   cp-tests
   join-tests
   semijoin-tests
   antijoin-tests
   union-tests
   intersect-tests
   except-tests
   outer-join-tests
   print-rel-tests
   range-overlaps-tests
   range-intersection-tests))

(module+ main
  (exit (if (zero? (run-tests all-tests)) 0 1)))

(module+ test
  (run-tests all-tests))
