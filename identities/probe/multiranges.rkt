#lang racket

;; Probe experiments for the multirange-based operators, the multirange
;; analogue of ranges.rkt. Same two identities, but each valid-at is a
;; multirange (a list of ranges), so some cases use genuinely multi-interval
;; valid-times.
;;
;; Run with: racket identities/probe/multiranges.rkt

(require "harness.rkt"
         "../../relsim.rkt")

(define Qd (tuple-desc '(q-id valid-at)))
(define Rd (tuple-desc '(r-id valid-at)))
(define Rd2 (tuple-desc '(r-id color valid-at)))
(define Qd2 (tuple-desc '(q-id grade valid-at)))

;; ===========================================================================
;; Experiment 1: does multirange-cartesian-product/drop-old distribute over
;; multirange-except?   Q × (R − S) = (Q × R) − (Q × S)
;; ===========================================================================
;;
;; Same reasoning as the range case (see ranges.rkt): /drop-old collapses both
;; inputs' valid-at columns to their single intersection, and multirange-except
;; unions all matching minus-multiranges per key before subtracting. The
;; multirange version is if anything simpler, since multirange-except keeps one
;; row per surviving input instead of splitting into one row per piece.

(define (drop-lhs Q R S)
  (multirange-cartesian-product/drop-old
   'valid-at Q (multirange-except 'valid-at R S)))

(define (drop-rhs Q R S)
  (multirange-except
   'valid-at
   (multirange-cartesian-product/drop-old 'valid-at Q R)
   (multirange-cartesian-product/drop-old 'valid-at Q S)))

(define drop-cases
  (list
   ;; the canonical one, singleton multiranges
   (list "canonical"
         (rel Qd (list (tuple 'q1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 10))))))
   ;; Q's valid-at is itself multi-interval
   (list "Q multi-interval valid-at"
         (rel Qd (list (tuple 'q1 '((0 . 5) (10 . 15)))))
         (rel Rd (list (tuple 'r1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 10))))))
   ;; R's valid-at is multi-interval, S punches a hole in the gap
   (list "R multi-interval; S in a gap"
         (rel Qd (list (tuple 'q1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 10) (20 . 30)))))
         (rel Rd (list (tuple 'r1 '((5 . 25))))))
   ;; two Q rows with same id but disjoint multiranges
   (list "Q two rows same id"
         (rel Qd (list (tuple 'q1 '((0 . 20))) (tuple 'q1 '((25 . 40)))))
         (rel Rd (list (tuple 'r1 '((0 . 40)))))
         (rel Rd (list (tuple 'r1 '((5 . 15))))))
   ;; R has duplicate rows
   (list "R has duplicates"
         (rel Qd (list (tuple 'q1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 30))) (tuple 'r1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((10 . 20))))))
   ;; multiple S rows that fully cover R
   (list "S fully covers R"
         (rel Qd (list (tuple 'q1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 15))) (tuple 'r1 '((10 . 30))))))
   ;; multiple Q rows + multiple S rows
   (list "Q×S with multiple rows on each side"
         (rel Qd (list (tuple 'q1 '((0 . 30))) (tuple 'q2 '((10 . 40)))))
         (rel Rd (list (tuple 'r1 '((0 . 40))) (tuple 'r2 '((5 . 35)))))
         (rel Rd (list (tuple 'r1 '((8 . 12))) (tuple 'r2 '((20 . 25))))))
   ;; empties
   (list "empty Q"
         (rel Qd '())
         (rel Rd (list (tuple 'r1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 10))))))
   (list "empty R"
         (rel Qd (list (tuple 'q1 '((0 . 20)))))
         (rel Rd '())
         (rel Rd (list (tuple 'r1 '((5 . 10))))))
   (list "empty S"
         (rel Qd (list (tuple 'q1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((0 . 20)))))
         (rel Rd '()))
   ;; an empty multirange valid-at (valid over no time)
   (list "empty-multirange valid-at"
         (rel Qd (list (tuple 'q1 '())))
         (rel Rd (list (tuple 'r1 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 10))))))
   ;; R/S with an extra non-time column
   (list "extra non-time column"
         (rel Qd (list (tuple 'q1 '((0 . 20)))))
         (rel Rd2 (list (tuple 'r1 "red" '((0 . 20))) (tuple 'r2 "blue" '((0 . 20)))))
         (rel Rd2 (list (tuple 'r1 "red" '((5 . 15))))))
   ;; Q with a non-time column
   (list "Q with non-time column"
         (rel Qd2 (list (tuple 'q1 'A '((0 . 20))) (tuple 'q1 'B '((10 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((5 . 25))))))
   ;; bag bombing: Q,R,S all dup the same row
   (list "Q,R,S all duplicate"
         (rel Qd (list (tuple 'q1 '((0 . 30))) (tuple 'q1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((0 . 30))) (tuple 'r1 '((0 . 30)))))
         (rel Rd (list (tuple 'r1 '((10 . 20))) (tuple 'r1 '((10 . 20))))))))

(define (rand-mr-rel desc id-syms n)
  (rel desc (for/list ([_ (in-range n)])
              (tuple (rand-elt id-syms) (rand-multirange)))))

(define (drop-gen)
  (list (rand-mr-rel Qd '(q1 q2 q3) (random 0 6))
        (rand-mr-rel Rd '(r1 r2 r3) (random 0 6))
        (rand-mr-rel Rd '(r1 r2 r3) (random 0 6))))

(probe "multirange-cartesian-product/drop-old distributes over multirange-except"
       drop-lhs drop-rhs drop-cases
       #:input-names '("Q" "R" "S")
       #:gen drop-gen #:trials 5000)

;; ===========================================================================
;; Experiment 2: does multirange-select distribute over
;; multirange-cartesian-product?
;;   σ_p(Q × R) = (σ_p Q) × R,  where p only inspects Q's columns
;; ===========================================================================

(define (q1? t) (eq? (list-ref (tuple-values t) 0) 'q1))

(define (sel-lhs Q R)
  (multirange-select q1? (multirange-cartesian-product 'valid-at Q R)))

(define (sel-rhs Q R)
  (multirange-cartesian-product 'valid-at (multirange-select q1? Q) R))

(define sel-cases
  (list
   (list "mixed q-ids, one R row"
         (rel Qd (list (tuple 'q1 '((0 . 20))) (tuple 'q2 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 15))))))
   (list "multi-interval valid-ats"
         (rel Qd (list (tuple 'q1 '((0 . 5) (10 . 20))) (tuple 'q2 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((3 . 12))))))
   (list "predicate keeps nothing"
         (rel Qd (list (tuple 'q2 '((0 . 20)))))
         (rel Rd (list (tuple 'r1 '((5 . 15))))))
   (list "empty R"
         (rel Qd (list (tuple 'q1 '((0 . 20)))))
         (rel Rd '()))))

(define (sel-gen)
  (list (rand-mr-rel Qd '(q1 q2 q3) (random 0 6))
        (rand-mr-rel Rd '(r1 r2 r3) (random 0 6))))

(probe "multirange-select distributes over multirange-cartesian-product"
       sel-lhs sel-rhs sel-cases
       #:input-names '("Q" "R")
       #:gen sel-gen #:trials 5000)
