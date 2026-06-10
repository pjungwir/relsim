#lang racket

;; Tests for the range-based temporal relational operators (range-relops.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide range-relops-suite)

(define range-join-tests
  (test-suite
   "range-join"
   ;; Two relations sharing an `id` and a `valid-at` range.
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(id role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '(0 . 10))
                         (tuple 2 "Bob"   '(5 . 15))))]
          [r2 (rel d2
                   (list (tuple 1 "eng"   '(3 . 7))
                         (tuple 1 "lead"  '(10 . 12)) ;; touches but no overlap
                         (tuple 2 "sales" '(0 . 8))))]
          [eq-id (lambda (a b)
                   (equal? (tuple-ref a d1 'id)
                           (tuple-ref b d2 'id)))])
     (test-case "matches rows with overlapping valid-times"
       (define r (range-join eq-id 'valid-at r1 r2))
       ;; Alice/eng overlap = (3 . 7); Bob/sales overlap = (5 . 8);
       ;; Alice/lead touches at 10 -> dropped.
       (check-equal? (length (rel-tuples r)) 2)
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (0 . 10) 1 "eng" (3 . 7) (3 . 7))
                       (2 "Bob"   (5 . 15) 2 "sales" (0 . 8) (5 . 8)))))
     (test-case "appends valid-attr to the result desc as an extra column"
       (define r (range-join eq-id 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at id role valid-at valid-at)))
     (test-case "drops rows when the predicate fails, even if ranges overlap"
       (define r (range-join (lambda (_ __) #f) 'valid-at r1 r2))
       (check-equal? (rel-tuples r) '()))
     (test-case "drops rows when ranges don't overlap, even if the predicate holds"
       (define r (range-join (lambda (_ __) #t) 'valid-at
                                (rel d1 (list (tuple 1 "Alice" '(0 . 5))))
                                (rel d2 (list (tuple 1 "eng"   '(5 . 9))))))
       (check-equal? (rel-tuples r) '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda ()
                    (range-join eq-id 'missing r1 r2)))))))

(define range-cartesian-product-tests
  (test-suite
   "range-cartesian-product"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '(0 . 10))
                         (tuple 2 "Bob"   '(20 . 30))))]
          [r2 (rel d2
                   (list (tuple "eng"   '(5 . 25))
                         (tuple "lead"  '(50 . 60))))])
     (test-case "pairs every overlapping (left,right) tuple"
       (define r (range-cartesian-product 'valid-at r1 r2))
       ;; Alice ∩ eng = [5,10); Bob ∩ eng = [20,25); 'lead' overlaps neither.
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (0 . 10) "eng" (5 . 25) (5 . 10))
                       (2 "Bob"   (20 . 30) "eng" (5 . 25) (20 . 25)))))
     (test-case "appends valid-attr to the result desc as an extra column"
       (define r (range-cartesian-product 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at valid-at)))
     (test-case "gives an empty result when either side is empty"
       (define empty-r (rel d1 '()))
       (check-equal? (rel-tuples
                      (range-cartesian-product 'valid-at empty-r r2))
                     '())
       (check-equal? (rel-tuples
                      (range-cartesian-product 'valid-at r1 (rel d2 '())))
                     '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda ()
                    (range-cartesian-product 'missing r1 r2)))))))

(define range-cartesian-product/overwrite-old-tests
  (test-suite
   "range-cartesian-product/overwrite-old"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '(0 . 10))
                         (tuple 2 "Bob"   '(20 . 30))))]
          [r2 (rel d2
                   (list (tuple "eng"  '(5 . 25))
                         (tuple "lead" '(50 . 60))))])
     (test-case "sets both inputs' valid-attr columns to the intersection"
       (define r (range-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (5 . 10) "eng" (5 . 10))
                       (2 "Bob"   (20 . 25) "eng" (20 . 25)))))
     (test-case "produces a result desc of left ++ right with no appended column"
       (define r (range-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at)))
     (test-case "drops non-overlapping pairs"
       (define a (rel d1 (list (tuple 1 "X" '(0 . 5)))))
       (define b (rel d2 (list (tuple "y" '(10 . 20)))))
       (define r (range-cartesian-product/overwrite-old 'valid-at a b))
       (check-equal? (rel-tuples r) '())))))

(define range-select-tests
  (test-suite
   "range-select"
   (let* ([d (tuple-desc '(id name valid-at))]
          [r (rel d
                  (list (tuple 1 "Alice" '(0 . 10))
                        (tuple 2 "Bob"   '(5 . 15))
                        (tuple 3 "Carol" '(20 . 30))))])
     (test-case "is an alias for select: the predicate filters rows"
       (define out (range-select
                    (lambda (t) (equal? (tuple-ref t d 'name) "Alice"))
                    r))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "leaves the desc unchanged"
       (define out (range-select (lambda (_) #t) r))
       (check-equal? (rel-desc out) d))
     (test-case "preserves valid-at values unchanged"
       (define out (range-select (lambda (_) #t) r))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10))
                       (2 "Bob"   (5 . 15))
                       (3 "Carol" (20 . 30))))))))

(define range-except-tests
  (test-suite
   "range-except"
   (let ([d (tuple-desc '(id name valid-at))])
     (test-case "subtracts overlapping range, splitting into two pieces"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 20)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(5 . 10)))))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 5))
                       (1 "Alice" (10 . 20)))))
     (test-case "ignores a non-matching key in r2"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10)))))
       (define r2 (rel d (list (tuple 2 "Bob" '(0 . 10)))))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "leaves the range intact when r2 only touches an endpoint"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(10 . 20)))))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "produces zero output rows when the range is fully covered"
       (define r1 (rel d (list (tuple 1 "Alice" '(5 . 8)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(0 . 20)))))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (rel-tuples out) '()))
     (test-case "accumulates multiple subtractions"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 30)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(5 . 10))
                               (tuple 1 "Alice" '(15 . 20)))))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 5))
                       (1 "Alice" (10 . 15))
                       (1 "Alice" (20 . 30)))))
     (test-case "returns r1 unchanged when r2 is empty"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10))
                               (tuple 2 "Bob"   '(5 . 15)))))
       (define r2 (rel d '()))
       (define out (range-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     (map tuple-values (rel-tuples r1))))
     (test-case "errors on mismatched descs"
       (check-exn exn:fail?
                  (lambda ()
                    (range-except 'valid-at
                                     (rel d '())
                                     (rel (tuple-desc '(id valid-at)) '())))))
     (test-case "errors when valid-attr is missing"
       (check-exn exn:fail?
                  (lambda ()
                    (range-except 'missing (rel d '()) (rel d '()))))))))

(define range-division-tests
  (test-suite
   "range-division"
   (let ([rd (tuple-desc '(sno pno valid-at))]
         [sd (tuple-desc '(pno valid-at))])
     (test-case "key is valid only while paired with every valid divisor value"
       ;; s1 supplies p1 over [0,10) but p2 only over [0,5); both required [0,10)
       (define R (rel rd (list (tuple 's1 'p1 '(0 . 10)) (tuple 's1 'p2 '(0 . 5)))))
       (define S (rel sd (list (tuple 'p1 '(0 . 10)) (tuple 'p2 '(0 . 10)))))
       (define out (range-division 'valid-at R S))
       (check-equal? (tuple-desc-fields (rel-desc out)) '(sno valid-at))
       (check-equal? (map tuple-values (rel-tuples out)) '((s1 (0 . 5)))))
     (test-case "splits a gappy result into one row per piece"
       (define R (rel rd (list (tuple 's1 'p1 '(0 . 5)) (tuple 's1 'p1 '(10 . 20)))))
       (define S (rel sd (list (tuple 'p1 '(0 . 20)))))
       (check-equal? (map tuple-values (rel-tuples (range-division 'valid-at R S)))
                     '((s1 (0 . 5)) (s1 (10 . 20)))))
     (test-case "two keys, each valid over its own qualifying window"
       (define R (rel rd (list (tuple 's1 'p1 '(0 . 10)) (tuple 's1 'p2 '(0 . 10))
                               (tuple 's2 'p1 '(0 . 10)) (tuple 's2 'p2 '(0 . 4)))))
       (define S (rel sd (list (tuple 'p1 '(0 . 10)) (tuple 'p2 '(0 . 10)))))
       (check-equal? (map tuple-values (rel-tuples (range-division 'valid-at R S)))
                     '((s1 (0 . 10)) (s2 (0 . 4)))))
     (test-case "agrees with Zimányi's temporal universal quantification (Case 2)"
       ;; Esteban Zimányi, "Temporal Aggregates and Temporal Universal
       ;; Quantification in Standard SQL", SIGMOD Record 35(2), 2006, Section 4,
       ;; Case 2 (Controls and WorksOn temporal). A worker qualifies exactly
       ;; while it works on every project its department currently controls.
       ;; Here the department controls p1 over [1,4) and p2 over [3,7); the
       ;; worker works p1 over [1,5) and p2 over [4,7). It loses the for-all on
       ;; [3,4), when p2 becomes required but is not yet worked.
       (define works (rel rd (list (tuple 's1 'p1 '(1 . 5))
                                   (tuple 's1 'p2 '(4 . 7)))))
       (define controlled (rel sd (list (tuple 'p1 '(1 . 4))
                                        (tuple 'p2 '(3 . 7)))))
       (check-equal? (map tuple-values
                          (rel-tuples (range-division 'valid-at works controlled)))
                     '((s1 (1 . 3)) (s1 (4 . 7)))))
     (test-case "empty divisor yields no rows"
       (define R (rel rd (list (tuple 's1 'p1 '(0 . 10)))))
       (check-equal? (rel-tuples (range-division 'valid-at R (rel sd '()))) '()))
     (test-case "errors when a divisor field is absent from the dividend"
       (check-exn exn:fail?
                  (lambda ()
                    (range-division 'valid-at (rel rd '())
                                    (rel (tuple-desc '(color valid-at)) '()))))))))

(define range-relops-suite
  (test-suite
   "range-relops"
   range-join-tests
   range-cartesian-product-tests
   range-cartesian-product/overwrite-old-tests
   range-select-tests
   range-except-tests
   range-division-tests))

(module+ main
  (exit (if (zero? (run-tests range-relops-suite)) 0 1)))

(module+ test
  (run-tests range-relops-suite))
