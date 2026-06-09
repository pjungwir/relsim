#lang racket

;; Tests for the range-based temporal relational operators (range-relops.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide range-relops-suite)

(define temporal-join-tests
  (test-suite
   "temporal-join"
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
       (define r (temporal-join eq-id 'valid-at r1 r2))
       ;; Alice/eng overlap = (3 . 7); Bob/sales overlap = (5 . 8);
       ;; Alice/lead touches at 10 -> dropped.
       (check-equal? (length (rel-tuples r)) 2)
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (0 . 10) 1 "eng" (3 . 7) (3 . 7))
                       (2 "Bob"   (5 . 15) 2 "sales" (0 . 8) (5 . 8)))))
     (test-case "result desc appends valid-attr as an extra column"
       (define r (temporal-join eq-id 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at id role valid-at valid-at)))
     (test-case "predicate failure drops rows even if ranges overlap"
       (define r (temporal-join (lambda (_ __) #f) 'valid-at r1 r2))
       (check-equal? (rel-tuples r) '()))
     (test-case "non-overlapping ranges drop rows even if pred holds"
       (define r (temporal-join (lambda (_ __) #t) 'valid-at
                                (rel d1 (list (tuple 1 "Alice" '(0 . 5))))
                                (rel d2 (list (tuple 1 "eng"   '(5 . 9))))))
       (check-equal? (rel-tuples r) '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda ()
                    (temporal-join eq-id 'missing r1 r2)))))))

(define temporal-cartesian-product-tests
  (test-suite
   "temporal-cartesian-product"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '(0 . 10))
                         (tuple 2 "Bob"   '(20 . 30))))]
          [r2 (rel d2
                   (list (tuple "eng"   '(5 . 25))
                         (tuple "lead"  '(50 . 60))))])
     (test-case "pairs every overlapping (left,right) tuple"
       (define r (temporal-cartesian-product 'valid-at r1 r2))
       ;; Alice ∩ eng = [5,10); Bob ∩ eng = [20,25); 'lead' overlaps neither.
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (0 . 10) "eng" (5 . 25) (5 . 10))
                       (2 "Bob"   (20 . 30) "eng" (5 . 25) (20 . 25)))))
     (test-case "result desc appends valid-attr as an extra column"
       (define r (temporal-cartesian-product 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at valid-at)))
     (test-case "empty rel on either side gives empty result"
       (define empty-r (rel d1 '()))
       (check-equal? (rel-tuples
                      (temporal-cartesian-product 'valid-at empty-r r2))
                     '())
       (check-equal? (rel-tuples
                      (temporal-cartesian-product 'valid-at r1 (rel d2 '())))
                     '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda ()
                    (temporal-cartesian-product 'missing r1 r2)))))))

(define temporal-cartesian-product/overwrite-old-tests
  (test-suite
   "temporal-cartesian-product/overwrite-old"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '(0 . 10))
                         (tuple 2 "Bob"   '(20 . 30))))]
          [r2 (rel d2
                   (list (tuple "eng"  '(5 . 25))
                         (tuple "lead" '(50 . 60))))])
     (test-case "both input valid-attr columns hold the intersection"
       (define r (temporal-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" (5 . 10) "eng" (5 . 10))
                       (2 "Bob"   (20 . 25) "eng" (20 . 25)))))
     (test-case "result desc is left ++ right with no appended column"
       (define r (temporal-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at)))
     (test-case "non-overlapping pairs are dropped"
       (define a (rel d1 (list (tuple 1 "X" '(0 . 5)))))
       (define b (rel d2 (list (tuple "y" '(10 . 20)))))
       (define r (temporal-cartesian-product/overwrite-old 'valid-at a b))
       (check-equal? (rel-tuples r) '())))))

(define temporal-select-tests
  (test-suite
   "temporal-select"
   (let* ([d (tuple-desc '(id name valid-at))]
          [r (rel d
                  (list (tuple 1 "Alice" '(0 . 10))
                        (tuple 2 "Bob"   '(5 . 15))
                        (tuple 3 "Carol" '(20 . 30))))])
     (test-case "is an alias for select: the predicate filters rows"
       (define out (temporal-select
                    (lambda (t) (equal? (tuple-ref t d 'name) "Alice"))
                    r))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "desc is unchanged"
       (define out (temporal-select (lambda (_) #t) r))
       (check-equal? (rel-desc out) d))
     (test-case "valid-at values are preserved unchanged"
       (define out (temporal-select (lambda (_) #t) r))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10))
                       (2 "Bob"   (5 . 15))
                       (3 "Carol" (20 . 30))))))))

(define temporal-except-tests
  (test-suite
   "temporal-except"
   (let ([d (tuple-desc '(id name valid-at))])
     (test-case "subtracts overlapping range, splitting into two pieces"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 20)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(5 . 10)))))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 5))
                       (1 "Alice" (10 . 20)))))
     (test-case "non-matching key in r2 is ignored"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10)))))
       (define r2 (rel d (list (tuple 2 "Bob" '(0 . 10)))))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "endpoint-only touch leaves range intact"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(10 . 20)))))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 10)))))
     (test-case "fully-covered range produces zero output rows"
       (define r1 (rel d (list (tuple 1 "Alice" '(5 . 8)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(0 . 20)))))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (rel-tuples out) '()))
     (test-case "multiple subtractions accumulate"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 30)))))
       (define r2 (rel d (list (tuple 1 "Alice" '(5 . 10))
                               (tuple 1 "Alice" '(15 . 20)))))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" (0 . 5))
                       (1 "Alice" (10 . 15))
                       (1 "Alice" (20 . 30)))))
     (test-case "empty r2 is identity"
       (define r1 (rel d (list (tuple 1 "Alice" '(0 . 10))
                               (tuple 2 "Bob"   '(5 . 15)))))
       (define r2 (rel d '()))
       (define out (temporal-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     (map tuple-values (rel-tuples r1))))
     (test-case "errors on mismatched descs"
       (check-exn exn:fail?
                  (lambda ()
                    (temporal-except 'valid-at
                                     (rel d '())
                                     (rel (tuple-desc '(id valid-at)) '())))))
     (test-case "errors when valid-attr is missing"
       (check-exn exn:fail?
                  (lambda ()
                    (temporal-except 'missing (rel d '()) (rel d '()))))))))

(define range-relops-suite
  (test-suite
   "range-relops"
   temporal-join-tests
   temporal-cartesian-product-tests
   temporal-cartesian-product/overwrite-old-tests
   temporal-select-tests
   temporal-except-tests))

(module+ main
  (exit (if (zero? (run-tests range-relops-suite)) 0 1)))

(module+ test
  (run-tests range-relops-suite))
