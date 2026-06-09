#lang racket

;; Tests for the TQuel temporal relational operators (tquel-relops.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide tquel-relops-suite)

(define tquel-conversion-tests
  (test-suite
   "rel<->tquel"
   (let* ([d (tuple-desc '(id name valid-at))]
          [r (rel d (list (tuple 1 "Alice" '(0 . 10))
                          (tuple 2 "Bob"   '(5 . 20))))])
     (test-case "converts a rel to TQuel form, dropping valid-attr and wrapping each field in a tsattr"
       (define sr (rel->tquel r 'valid-at))
       (check-equal? (tuple-desc-fields (rel-desc sr)) '(id name))
       (check-equal? (map tuple-values (rel-tuples sr))
                     (list (list (tsattr 1 '((0 . 10))) (tsattr "Alice" '((0 . 10))))
                           (list (tsattr 2 '((5 . 20))) (tsattr "Bob"   '((5 . 20)))))))
     (test-case "round-trips a rel through TQuel form unchanged when valid-ats are uniform"
       (define sr (rel->tquel r 'valid-at))
       (define r* (tquel->rel sr 'valid-at))
       (check-equal? (tuple-desc-fields (rel-desc r*)) '(id name valid-at))
       (check-equal? (map tuple-values (rel-tuples r*))
                     '((1 "Alice" (0 . 10))
                       (2 "Bob"   (5 . 20))))))
   (test-case "converts back to a rel, splitting at every attribute endpoint and NULLing where an attribute isn't valid"
     (define sr (rel (tuple-desc '(a b))
                     (list (tuple (tsattr 'x '((0 . 5) (10 . 15)))
                                  (tsattr 'y '((2 . 12)))))))
     (define r (tquel->rel sr 'valid-at))
     (check-equal? (map tuple-values (rel-tuples r))
                   '((x  ()  (0 . 2))
                     (x  y   (2 . 5))
                     (()  y  (5 . 10))
                     (x  y   (10 . 12))
                     (x  ()  (12 . 15)))))
   (test-case "converts back to a rel, keeping a tuple even when its attributes' valid-ats don't overlap"
     (define sr (rel (tuple-desc '(a b))
                     (list (tuple (tsattr 'x '((0 . 5)))
                                  (tsattr 'y '((10 . 15)))))))
     (check-equal? (map tuple-values (rel-tuples (tquel->rel sr 'valid-at)))
                   '((x  ()  (0 . 5))
                     (()  y  (10 . 15)))))))

(define temporal-union/tquel-tests
  (test-suite
   "temporal-union/tquel"
   (let* ([d (tuple-desc '(a b))]
          [r1 (rel d (list (tuple (tsattr 1 '((0 . 5)))
                                  (tsattr 'x '((0 . 5))))))]
          [r2 (rel d (list (tuple (tsattr 1 '((10 . 15)))
                                  (tsattr 'x '((10 . 15))))))])
     (test-case "merges rows with identical values by unioning each attribute's valid-at"
       (define u (temporal-union/tquel r1 r2))
       (check-equal? (map tuple-values (rel-tuples u))
                     (list (list (tsattr 1 '((0 . 5) (10 . 15)))
                                 (tsattr 'x '((0 . 5) (10 . 15)))))))
     (test-case "keeps rows with differing values as separate rows"
       (define r3 (rel d (list (tuple (tsattr 2 '((0 . 5)))
                                      (tsattr 'y '((0 . 5)))))))
       (define u (temporal-union/tquel r1 r3))
       (check-equal? (length (rel-tuples u)) 2))
     (test-case "errors on desc mismatch"
       (check-exn exn:fail?
                  (lambda ()
                    (temporal-union/tquel
                     r1 (rel (tuple-desc '(a)) '()))))))))

(define temporal-except/tquel-tests
  (test-suite
   "temporal-except/tquel"
   (let* ([d (tuple-desc '(id name))])
     (test-case "subtracts each attribute's valid-at from the row with matching values"
       (define r1 (rel d (list (tuple (tsattr 1 '((0 . 20)))
                                      (tsattr "Alice" '((0 . 20)))))))
       (define r2 (rel d (list (tuple (tsattr 1 '((5 . 10)))
                                      (tsattr "Alice" '((5 . 10)))))))
       (define diff (temporal-except/tquel r1 r2))
       (check-equal? (map tuple-values (rel-tuples diff))
                     (list (list (tsattr 1 '((0 . 5) (10 . 20)))
                                 (tsattr "Alice" '((0 . 5) (10 . 20)))))))
     (test-case "drops a row only when every attribute's valid-at becomes empty"
       (define r1 (rel d (list (tuple (tsattr 1 '((0 . 10)))
                                      (tsattr "Alice" '((0 . 10)))))))
       (define r2 (rel d (list (tuple (tsattr 1 '((0 . 10)))
                                      (tsattr "Alice" '((0 . 10)))))))
       (check-equal? (rel-tuples (temporal-except/tquel r1 r2)) '()))
     (test-case "keeps a row when at least one attribute still has a valid-at"
       (define r1 (rel d (list (tuple (tsattr 1 '((0 . 10)))
                                      (tsattr "Alice" '((0 . 20)))))))
       (define r2 (rel d (list (tuple (tsattr 1 '((0 . 10)))
                                      (tsattr "Alice" '((0 . 5)))))))
       (define diff (temporal-except/tquel r1 r2))
       (check-equal? (map tuple-values (rel-tuples diff))
                     (list (list (tsattr 1 '())
                                 (tsattr "Alice" '((5 . 20)))))))
     (test-case "passes a row through unchanged when no row has matching values"
       (define r1 (rel d (list (tuple (tsattr 1 '((0 . 20)))
                                      (tsattr "Alice" '((0 . 20)))))))
       (define r2 (rel d (list (tuple (tsattr 2 '((5 . 10)))
                                      (tsattr "Bob"   '((5 . 10)))))))
       (define diff (temporal-except/tquel r1 r2))
       (check-equal? (length (rel-tuples diff)) 1)))))

(define temporal-cartesian-product/tquel-tests
  (test-suite
   "temporal-cartesian-product/tquel"
   (test-case "is plain cartesian-product over tsattr-valued tuples"
     (define d1 (tuple-desc '(a)))
     (define d2 (tuple-desc '(b)))
     (define r1 (rel d1 (list (tuple (tsattr 1 '((0 . 10))))
                              (tuple (tsattr 2 '((5 . 15)))))))
     (define r2 (rel d2 (list (tuple (tsattr 'x '((0 . 8)))))))
     (define p (temporal-cartesian-product/tquel r1 r2))
     (check-equal? (tuple-desc-fields (rel-desc p)) '(a b))
     (check-equal? (length (rel-tuples p)) 2)
     (check-equal? (map tuple-values (rel-tuples p))
                   (list (list (tsattr 1 '((0 . 10))) (tsattr 'x '((0 . 8))))
                         (list (tsattr 2 '((5 . 15))) (tsattr 'x '((0 . 8)))))))))

(define temporal-select/tquel-tests
  (test-suite
   "temporal-select/tquel"
   (test-case "filters on a predicate over the whole tuple"
     (define d (tuple-desc '(id name)))
     (define r (rel d (list (tuple (tsattr 1 '((0 . 10)))
                                   (tsattr "Alice" '((0 . 10))))
                            (tuple (tsattr 2 '((0 . 10)))
                                   (tsattr "Bob"   '((0 . 10)))))))
     (define out (temporal-select/tquel
                  (lambda (t)
                    (= (tsattr-val (list-ref (tuple-values t) 0)) 1))
                  r))
     (check-equal? (length (rel-tuples out)) 1)
     (check-equal? (tsattr-val (list-ref (tuple-values (car (rel-tuples out))) 0))
                   1))))

(define temporal-project/tquel-tests
  (test-suite
   "temporal-project/tquel"
   (test-case "drops unlisted fields and merges same-val survivors"
     (define d (tuple-desc '(dept name)))
     (define r (rel d
                    (list (tuple (tsattr "Eng"   '((0 . 10)))
                                 (tsattr "Alice" '((0 . 10))))
                          (tuple (tsattr "Eng"   '((10 . 20)))
                                 (tsattr "Bob"   '((10 . 20)))))))
     (define out (temporal-project/tquel '(dept) r))
     (check-equal? (tuple-desc-fields (rel-desc out)) '(dept))
     (check-equal? (map tuple-values (rel-tuples out))
                   (list (list (tsattr "Eng" '((0 . 20)))))))
   (test-case "preserves separate tuples for different vals"
     (define d (tuple-desc '(dept name)))
     (define r (rel d
                    (list (tuple (tsattr "Eng"   '((0 . 10)))
                                 (tsattr "Alice" '((0 . 10))))
                          (tuple (tsattr "Sales" '((0 . 10)))
                                 (tsattr "Bob"   '((0 . 10)))))))
     (define out (temporal-project/tquel '(dept) r))
     (check-equal? (length (rel-tuples out)) 2))
   (test-case "keeps a row when at least one kept attribute has a valid-at"
     (define d (tuple-desc '(id name)))
     (define r (rel d (list (tuple (tsattr 1   '())
                                    (tsattr "Alice" '((0 . 10)))))))
     (define out (temporal-project/tquel '(id name) r))
     (check-equal? (map tuple-values (rel-tuples out))
                   (list (list (tsattr 1 '())
                               (tsattr "Alice" '((0 . 10)))))))
   (test-case "drops a row when every kept attribute has an empty valid-at"
     (define d (tuple-desc '(id name)))
     (define r (rel d (list (tuple (tsattr 1   '())
                                    (tsattr "Alice" '((0 . 10)))))))
     ;; Project keeps only the id attribute, but its valid-at is empty,
     ;; so we drop the tuple.
     (define out (temporal-project/tquel '(id) r))
     (check-equal? (rel-tuples out) '()))))

(define tquel-relops-suite
  (test-suite
   "tquel-relops"
   tquel-conversion-tests
   temporal-union/tquel-tests
   temporal-except/tquel-tests
   temporal-cartesian-product/tquel-tests
   temporal-select/tquel-tests
   temporal-project/tquel-tests))

(module+ main
  (exit (if (zero? (run-tests tquel-relops-suite)) 0 1)))

(module+ test
  (run-tests tquel-relops-suite))
