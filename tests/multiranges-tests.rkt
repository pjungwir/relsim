#lang racket

;; Tests for the multirange utility functions (multiranges.rkt).

(require rackunit
         rackunit/text-ui
         "../relsim.rkt")

(provide multiranges-suite)

(define multirange-tests
  (test-suite
   "multirange"
   (test-case "canonicalizes a multirange by sorting and merging overlapping or touching members"
     (check-equal? (multirange-canonical '((5 . 8) (0 . 3)))
                   '((0 . 3) (5 . 8)))
     (check-equal? (multirange-canonical '((0 . 5) (3 . 8)))
                   '((0 . 8)))
     (check-equal? (multirange-canonical '((0 . 5) (5 . 10)))
                   '((0 . 10)))
     (check-equal? (multirange-canonical '()) '()))
   (test-case "unions two multiranges, merging where they meet"
     (check-equal? (multirange-union '((0 . 5)) '((10 . 15)))
                   '((0 . 5) (10 . 15)))
     (check-equal? (multirange-union '((0 . 5)) '((3 . 10)))
                   '((0 . 10))))
   (test-case "intersects two multiranges"
     (check-equal? (multirange-intersection '((0 . 10)) '((5 . 15)))
                   '((5 . 10)))
     (check-equal? (multirange-intersection '((0 . 5) (10 . 15))
                                            '((3 . 12)))
                   '((3 . 5) (10 . 12)))
     (check-equal? (multirange-intersection '((0 . 5)) '((10 . 15)))
                   '()))
   (test-case "subtracts one multirange from another"
     (check-equal? (multirange-subtract '((0 . 20)) '((5 . 10)))
                   '((0 . 5) (10 . 20)))
     (check-equal? (multirange-subtract '((0 . 20)) '((5 . 10) (12 . 15)))
                   '((0 . 5) (10 . 12) (15 . 20)))
     (check-equal? (multirange-subtract '((0 . 5)) '((0 . 5))) '()))
   (test-case "reports whether two multiranges share any point"
     (check-true  (multirange-overlaps? '((0 . 5)) '((3 . 8))))
     (check-false (multirange-overlaps? '((0 . 5)) '((5 . 10))))
     (check-false (multirange-overlaps? '() '((0 . 10)))))
   (test-case "recognises only the empty multirange"
     (check-true  (multirange-empty? '()))
     (check-false (multirange-empty? '((0 . 1)))))))

(define multiranges-suite
  (test-suite
   "multiranges"
   multirange-tests))

(module+ main
  (exit (if (zero? (run-tests multiranges-suite)) 0 1)))

(module+ test
  (run-tests multiranges-suite))
