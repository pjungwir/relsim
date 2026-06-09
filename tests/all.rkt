#lang racket

;; Aggregate test runner. Pulls in every per-category suite and runs them
;; under one umbrella. Run with `racket tests/all.rkt` (exits 0/1) or
;; `raco test tests/all.rkt`. Individual files are runnable on their own too,
;; e.g. `racket tests/relops-tests.rkt`, or run the whole folder with
;; `raco test tests/`.

(require rackunit
         rackunit/text-ui
         "core-tests.rkt"
         "ranges-tests.rkt"
         "multiranges-tests.rkt"
         "relops-tests.rkt"
         "range-relops-tests.rkt"
         "multirange-relops-tests.rkt"
         "tquel-relops-tests.rkt")

(define all-tests
  (test-suite
   "relsim"
   core-suite
   ranges-suite
   multiranges-suite
   relops-suite
   range-relops-suite
   multirange-relops-suite
   tquel-relops-suite))

(module+ main
  (exit (if (zero? (run-tests all-tests)) 0 1)))

(module+ test
  (run-tests all-tests))
