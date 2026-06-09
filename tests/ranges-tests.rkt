#lang racket

;; Tests for the range utility functions (ranges.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide ranges-suite)

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

(define ranges-suite
  (test-suite
   "ranges"
   range-overlaps-tests
   range-intersection-tests))

(module+ main
  (exit (if (zero? (run-tests ranges-suite)) 0 1)))

(module+ test
  (run-tests ranges-suite))
