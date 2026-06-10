#lang racket

;; Tests for the multirange-based temporal relational operators
;; (multirange-relops.rkt). Each tuple's valid-at is a multirange (a list of
;; ranges) rather than a single range.

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide multirange-relops-suite)

(define multirange-join-tests
  (test-suite
   "multirange-join"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(id role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '((0 . 10)))
                         (tuple 2 "Bob"   '((5 . 15)))))]
          [r2 (rel d2
                   (list (tuple 1 "eng"   '((3 . 7)))
                         (tuple 1 "lead"  '((10 . 12))) ;; touches but no overlap
                         (tuple 2 "sales" '((0 . 8)))))]
          [eq-id (lambda (a b)
                   (equal? (tuple-ref a d1 'id)
                           (tuple-ref b d2 'id)))])
     (test-case "matches rows whose valid-time multiranges overlap"
       (define r (multirange-join eq-id 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" ((0 . 10)) 1 "eng" ((3 . 7)) ((3 . 7)))
                       (2 "Bob"   ((5 . 15)) 2 "sales" ((0 . 8)) ((5 . 8))))))
     (test-case "appends valid-attr to the result desc as an extra column"
       (define r (multirange-join eq-id 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at id role valid-at valid-at)))
     (test-case "drops rows when the predicate fails, even if multiranges overlap"
       (define r (multirange-join (lambda (_ __) #f) 'valid-at r1 r2))
       (check-equal? (rel-tuples r) '()))
     (test-case "drops rows when multiranges only touch at an endpoint"
       (define r (multirange-join (lambda (_ __) #t) 'valid-at
                                  (rel d1 (list (tuple 1 "Alice" '((0 . 5)))))
                                  (rel d2 (list (tuple 1 "eng"   '((5 . 9)))))))
       (check-equal? (rel-tuples r) '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda () (multirange-join eq-id 'missing r1 r2)))))))

(define multirange-cartesian-product-tests
  (test-suite
   "multirange-cartesian-product"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '((0 . 10)))
                         (tuple 2 "Bob"   '((20 . 30)))))]
          [r2 (rel d2
                   (list (tuple "eng"   '((5 . 25)))
                         (tuple "lead"  '((50 . 60)))))])
     (test-case "pairs every overlapping (left,right) tuple"
       (define r (multirange-cartesian-product 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" ((0 . 10)) "eng" ((5 . 25)) ((5 . 10)))
                       (2 "Bob"   ((20 . 30)) "eng" ((5 . 25)) ((20 . 25))))))
     (test-case "intersects each side into a multi-interval valid-at"
       ;; Left valid-at has a gap; the intersection keeps both pieces.
       (define a (rel d1 (list (tuple 1 "Alice" '((0 . 5) (10 . 15))))))
       (define b (rel d2 (list (tuple "eng" '((3 . 12))))))
       (define r (multirange-cartesian-product 'valid-at a b))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" ((0 . 5) (10 . 15)) "eng" ((3 . 12))
                          ((3 . 5) (10 . 12))))))
     (test-case "appends valid-attr to the result desc as an extra column"
       (define r (multirange-cartesian-product 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at valid-at)))
     (test-case "gives an empty result when either side is empty"
       (check-equal? (rel-tuples
                      (multirange-cartesian-product 'valid-at (rel d1 '()) r2))
                     '())
       (check-equal? (rel-tuples
                      (multirange-cartesian-product 'valid-at r1 (rel d2 '())))
                     '()))
     (test-case "errors when valid-attr is missing from a side"
       (check-exn exn:fail?
                  (lambda ()
                    (multirange-cartesian-product 'missing r1 r2)))))))

(define multirange-cartesian-product/overwrite-old-tests
  (test-suite
   "multirange-cartesian-product/overwrite-old"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1
                   (list (tuple 1 "Alice" '((0 . 10)))
                         (tuple 2 "Bob"   '((20 . 30)))))]
          [r2 (rel d2
                   (list (tuple "eng"  '((5 . 25)))
                         (tuple "lead" '((50 . 60)))))])
     (test-case "sets both inputs' valid-attr columns to the intersection"
       (define r (multirange-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" ((5 . 10)) "eng" ((5 . 10)))
                       (2 "Bob"   ((20 . 25)) "eng" ((20 . 25))))))
     (test-case "produces a result desc of left ++ right with no appended column"
       (define r (multirange-cartesian-product/overwrite-old 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name valid-at role valid-at)))
     (test-case "drops non-overlapping pairs"
       (define a (rel d1 (list (tuple 1 "X" '((0 . 5))))))
       (define b (rel d2 (list (tuple "y" '((10 . 20))))))
       (define r (multirange-cartesian-product/overwrite-old 'valid-at a b))
       (check-equal? (rel-tuples r) '())))))

(define multirange-cartesian-product/drop-old-tests
  (test-suite
   "multirange-cartesian-product/drop-old"
   (let* ([d1 (tuple-desc '(id name valid-at))]
          [d2 (tuple-desc '(role valid-at))]
          [r1 (rel d1 (list (tuple 1 "Alice" '((0 . 10)))))]
          [r2 (rel d2 (list (tuple "eng" '((5 . 25)))))])
     (test-case "drops the left valid-attr and sets the right to the intersection"
       (define r (multirange-cartesian-product/drop-old 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples r))
                     '((1 "Alice" "eng" ((5 . 10))))))
     (test-case "produces a result desc with the left valid-attr removed"
       (define r (multirange-cartesian-product/drop-old 'valid-at r1 r2))
       (check-equal? (tuple-desc-fields (rel-desc r))
                     '(id name role valid-at))))))

(define multirange-select-tests
  (test-suite
   "multirange-select"
   (let* ([d (tuple-desc '(id name valid-at))]
          [r (rel d
                  (list (tuple 1 "Alice" '((0 . 10)))
                        (tuple 2 "Bob"   '((5 . 15)))))])
     (test-case "is an alias for select: the predicate filters rows"
       (define out (multirange-select
                    (lambda (t) (equal? (tuple-ref t d 'name) "Alice"))
                    r))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" ((0 . 10))))))
     (test-case "leaves valid-at multiranges unchanged"
       (define out (multirange-select (lambda (_) #t) r))
       (check-equal? (map tuple-values (rel-tuples out))
                     (map tuple-values (rel-tuples r)))))))

(define multirange-except-tests
  (test-suite
   "multirange-except"
   (let ([d (tuple-desc '(id name valid-at))])
     (test-case "subtracts the overlap, leaving one multi-interval row"
       (define r1 (rel d (list (tuple 1 "Alice" '((0 . 20))))))
       (define r2 (rel d (list (tuple 1 "Alice" '((5 . 10))))))
       (define out (multirange-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" ((0 . 5) (10 . 20))))))
     (test-case "subtracts from a minuend that is already multi-interval"
       (define r1 (rel d (list (tuple 1 "Alice" '((0 . 10) (20 . 30))))))
       (define r2 (rel d (list (tuple 1 "Alice" '((5 . 25))))))
       (define out (multirange-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" ((0 . 5) (25 . 30))))))
     (test-case "ignores a non-matching key in r2"
       (define r1 (rel d (list (tuple 1 "Alice" '((0 . 10))))))
       (define r2 (rel d (list (tuple 2 "Bob" '((0 . 10))))))
       (define out (multirange-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" ((0 . 10))))))
     (test-case "produces no row when the multirange is fully covered"
       (define r1 (rel d (list (tuple 1 "Alice" '((5 . 8))))))
       (define r2 (rel d (list (tuple 1 "Alice" '((0 . 20))))))
       (define out (multirange-except 'valid-at r1 r2))
       (check-equal? (rel-tuples out) '()))
     (test-case "unions multiple matching r2 rows before subtracting"
       (define r1 (rel d (list (tuple 1 "Alice" '((0 . 30))))))
       (define r2 (rel d (list (tuple 1 "Alice" '((5 . 10)))
                               (tuple 1 "Alice" '((15 . 20))))))
       (define out (multirange-except 'valid-at r1 r2))
       (check-equal? (map tuple-values (rel-tuples out))
                     '((1 "Alice" ((0 . 5) (10 . 15) (20 . 30))))))
     (test-case "returns r1 unchanged when r2 is empty"
       (define r1 (rel d (list (tuple 1 "Alice" '((0 . 10)))
                               (tuple 2 "Bob"   '((5 . 15))))))
       (define out (multirange-except 'valid-at r1 (rel d '())))
       (check-equal? (map tuple-values (rel-tuples out))
                     (map tuple-values (rel-tuples r1))))
     (test-case "errors on mismatched descs"
       (check-exn exn:fail?
                  (lambda ()
                    (multirange-except 'valid-at
                                       (rel d '())
                                       (rel (tuple-desc '(id valid-at)) '())))))
     (test-case "errors when valid-attr is missing"
       (check-exn exn:fail?
                  (lambda ()
                    (multirange-except 'missing (rel d '()) (rel d '()))))))))

(define multirange-division-tests
  (test-suite
   "multirange-division"
   (let ([rd (tuple-desc '(sno pno valid-at))]
         [sd (tuple-desc '(pno valid-at))])
     (test-case "key is valid only while paired with every valid divisor value"
       (define R (rel rd (list (tuple 's1 'p1 '((0 . 10))) (tuple 's1 'p2 '((0 . 5))))))
       (define S (rel sd (list (tuple 'p1 '((0 . 10))) (tuple 'p2 '((0 . 10))))))
       (define out (multirange-division 'valid-at R S))
       (check-equal? (tuple-desc-fields (rel-desc out)) '(sno valid-at))
       (check-equal? (map tuple-values (rel-tuples out)) '((s1 ((0 . 5))))))
     (test-case "keeps a gappy result as one multi-interval row"
       (define R (rel rd (list (tuple 's1 'p1 '((0 . 5))) (tuple 's1 'p1 '((10 . 20))))))
       (define S (rel sd (list (tuple 'p1 '((0 . 20))))))
       (check-equal? (map tuple-values (rel-tuples (multirange-division 'valid-at R S)))
                     '((s1 ((0 . 5) (10 . 20))))))
     (test-case "multi-interval divisor: result is intersected with its lifespan"
       ;; p1 required over [0,10) and [20,30); s1 supplies p1 over [0,25)
       (define R (rel rd (list (tuple 's1 'p1 '((0 . 25))))))
       (define S (rel sd (list (tuple 'p1 '((0 . 10) (20 . 30))))))
       (check-equal? (map tuple-values (rel-tuples (multirange-division 'valid-at R S)))
                     '((s1 ((0 . 10) (20 . 25))))))
     (test-case "agrees with Zimányi's temporal universal quantification (Case 2)"
       ;; Esteban Zimányi, "Temporal Aggregates and Temporal Universal
       ;; Quantification in Standard SQL", SIGMOD Record 35(2), 2006, Section 4,
       ;; Case 2. Same scenario as the range-division test, but the gappy
       ;; for-all stays one multi-interval row instead of splitting.
       (define works (rel rd (list (tuple 's1 'p1 '((1 . 5)))
                                   (tuple 's1 'p2 '((4 . 7))))))
       (define controlled (rel sd (list (tuple 'p1 '((1 . 4)))
                                        (tuple 'p2 '((3 . 7))))))
       (check-equal? (map tuple-values
                          (rel-tuples (multirange-division 'valid-at works controlled)))
                     '((s1 ((1 . 3) (4 . 7))))))
     (test-case "empty divisor yields no rows"
       (define R (rel rd (list (tuple 's1 'p1 '((0 . 10))))))
       (check-equal? (rel-tuples (multirange-division 'valid-at R (rel sd '()))) '()))
     (test-case "errors when a divisor field is absent from the dividend"
       (check-exn exn:fail?
                  (lambda ()
                    (multirange-division 'valid-at (rel rd '())
                                         (rel (tuple-desc '(color valid-at)) '()))))))))

(define multirange-relops-suite
  (test-suite
   "multirange-relops"
   multirange-join-tests
   multirange-cartesian-product-tests
   multirange-cartesian-product/overwrite-old-tests
   multirange-cartesian-product/drop-old-tests
   multirange-select-tests
   multirange-except-tests
   multirange-division-tests))

(module+ main
  (exit (if (zero? (run-tests multirange-relops-suite)) 0 1)))

(module+ test
  (run-tests multirange-relops-suite))
