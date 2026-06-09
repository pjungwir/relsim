#lang racket

;; Tests for the range utility functions (ranges.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide ranges-suite)

(define range-overlaps-tests
  (test-suite
   "range-overlaps"
   (test-case "reports overlap when ranges share interior points"
     (check-true  (range-overlaps '(1 . 5) '(3 . 7)))
     (check-true  (range-overlaps '(3 . 7) '(1 . 5)))
     (check-true  (range-overlaps '(1 . 10) '(3 . 5)))   ;; one contains the other
     (check-true  (range-overlaps '(3 . 5) '(1 . 10))))
   (test-case "reports no overlap when ranges only touch at an endpoint"
     (check-false (range-overlaps '(1 . 3) '(3 . 5)))
     (check-false (range-overlaps '(3 . 5) '(1 . 3))))
   (test-case "reports no overlap for fully disjoint ranges"
     (check-false (range-overlaps '(1 . 2) '(5 . 9)))
     (check-false (range-overlaps '(5 . 9) '(1 . 2))))
   (test-case "works with non-integer comparable values"
     (check-true  (range-overlaps '(1.5 . 3.5) '(2.0 . 4.0)))
     (check-false (range-overlaps '(1.5 . 2.0) '(2.0 . 3.0))))))

(define range-intersection-tests
  (test-suite
   "range-intersection"
   (test-case "returns the shared sub-range for a partial overlap"
     (check-equal? (range-intersection '(1 . 5) '(3 . 7)) '(3 . 5))
     (check-equal? (range-intersection '(3 . 7) '(1 . 5)) '(3 . 5)))
   (test-case "returns the inner range when one range contains the other"
     (check-equal? (range-intersection '(1 . 10) '(3 . 5)) '(3 . 5))
     (check-equal? (range-intersection '(3 . 5) '(1 . 10)) '(3 . 5)))
   (test-case "returns the same range when both inputs are identical"
     (check-equal? (range-intersection '(2 . 8) '(2 . 8)) '(2 . 8)))
   (test-case "returns #f for endpoint-touching ranges"
     (check-false (range-intersection '(1 . 3) '(3 . 5)))
     (check-false (range-intersection '(3 . 5) '(1 . 3))))
   (test-case "returns #f for fully disjoint ranges"
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
