#lang racket

;; Tests for the ordinary (non-temporal) relational operators (relops.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide relops-suite)

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

(define select-tests
  (test-suite
   "Select"
   (test-case "filters rows by predicate"
     (define r (select (lambda (t)
                         (equal? (tuple-ref t employees-desc 'dept-id) 10))
                       employees))
     (check-equal? (length (rel-tuples r)) 2)
     (check-equal? (rel-desc r) employees-desc))
   (test-case "yields an empty rel when the predicate is always false"
     (define r (select (lambda (_) #f) employees))
     (check-equal? (rel-tuples r) '()))
   (test-case "preserves duplicate rows"
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
   (test-case "produces a row count equal to the product of the inputs"
     (define r (cartesian-product employees depts))
     (check-equal? (length (rel-tuples r)) (* 4 3)))
   (test-case "concatenates the input fields in its desc"
     (define r (cartesian-product employees depts))
     (check-equal? (tuple-desc-fields (rel-desc r))
                   '(id name dept-id dept-id dept-name)))
   (test-case "gives an empty product when either side is empty"
     (define empty-r (rel employees-desc '()))
     (check-equal? (rel-tuples (cartesian-product empty-r depts)) '())
     (check-equal? (rel-tuples (cartesian-product depts empty-r)) '()))))

(define join-tests
  (test-suite
   "Join"
   (test-case "matches rows on dept-id for an equi-join"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define r (join (lambda (e d)
                       (equal? (tuple-ref e ed 'dept-id)
                               (tuple-ref d dd 'dept-id)))
                     employees depts))
     ;; Alice+Eng, Bob+Sales, Carol+Eng -> 3 rows. Dan (null) and HR drop out.
     (check-equal? (length (rel-tuples r)) 3))
   (test-case "behaves like cartesian product when the predicate is always true"
     (define r (join (lambda (_ __) #t) employees depts))
     (check-equal? (length (rel-tuples r)) 12))
   (test-case "concatenates the input fields in its desc"
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
   (test-case "returns all left rows when the right rel is empty"
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
   (test-case "keeps min(m,n) copies of each tuple (multiset intersection)"
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
   (test-case "keeps max(m-n, 0) copies of each tuple (multiset difference)"
     (define a (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 1) (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(x))
                    (list (tuple 1) (tuple 3))))
     (define r (except a b))
     ;; (1) 3-1=2; (2) 1-0=1.
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1) (1) (2))))
   (test-case "returns the left rel unchanged when the right is empty"
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
   (test-case "keeps unmatched rows on both sides, padded with nulls"
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
   (test-case "drops unmatched right rows for a left outer join"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define pred (lambda (e d)
                    (equal? (tuple-ref e ed 'dept-id)
                            (tuple-ref d dd 'dept-id))))
     (define r (outer-join pred employees depts #:side 'left))
     ;; 3 matches + Dan = 4
     (check-equal? (length (rel-tuples r)) 4))
   (test-case "drops unmatched left rows for a right outer join"
     (define ed (rel-desc employees))
     (define dd (rel-desc depts))
     (define pred (lambda (e d)
                    (equal? (tuple-ref e ed 'dept-id)
                            (tuple-ref d dd 'dept-id))))
     (define r (outer-join pred employees depts #:side 'right))
     ;; 3 matches + HR = 4
     (check-equal? (length (rel-tuples r)) 4))
   (test-case "returns left-padded and right-padded rows when nothing matches"
     (define a (rel (tuple-desc '(x)) (list (tuple 1) (tuple 2))))
     (define b (rel (tuple-desc '(y)) (list (tuple 9))))
     (define r (outer-join (lambda (_ __) #f) a b #:side 'full))
     (check-equal? (length (rel-tuples r)) 3)
     (check-equal? (map tuple-values (rel-tuples r))
                   '((1 ()) (2 ()) (() 9))))))

(define relops-suite
  (test-suite
   "relops"
   select-tests
   project-tests
   cp-tests
   join-tests
   semijoin-tests
   antijoin-tests
   union-tests
   intersect-tests
   except-tests
   outer-join-tests))

(module+ main
  (exit (if (zero? (run-tests relops-suite)) 0 1)))

(module+ test
  (run-tests relops-suite))
