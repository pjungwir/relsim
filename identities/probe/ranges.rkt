#lang racket

;; Probe experiments for the range-based operators, run through the shared
;; harness in harness.rkt. Each `probe` call states an identity as an LHS and
;; RHS procedure and checks it over hand-picked cases plus random fuzz.
;;
;; Run with: racket identities/probe/ranges.rkt

(require "harness.rkt"
         "../../relsim.rkt")

(define Qd (tuple-desc '(q-id valid-at)))
(define Rd (tuple-desc '(r-id valid-at)))
(define Rd2 (tuple-desc '(r-id color valid-at)))
(define Qd2 (tuple-desc '(q-id grade valid-at)))

;; ===========================================================================
;; Experiment 1: does range-cartesian-product/drop-old distribute over
;; range-except?   Q × (R − S) = (Q × R) − (Q × S)
;; ===========================================================================
;;
;; Why we expect it to hold (proof sketch). For a given output key (q, r),
;; let
;;   Q_q  = ⋃ { qt  : (q, qt)  ∈ Q }
;;   S_r  = ⋃ { st  : (r, st)  ∈ S }
;; In other words, Q_q is all the valid-times that q appeared in Q,
;; and S_r is all the valid-times that r appeared in S.
;;
;; The surviving valid-time set contributed by an input (qt, rt) pair is:
;;
;;   LHS  Q × (R − S)        :  qt ∩ (rt − S_r)
;;                          = (qt ∩ rt) − S_r
;;     Intersect and different associate: If we start with qt, then keep what's
;;     in rt and lose what's in S_r, it doesn't matter the order of operations.
;;
;;   RHS  (Q × R) − (Q × S)  :  (qt ∩ rt) − M
;;     where M is the union, over the range-except key (q, r), of all
;;     (qt' ∩ st) values produced by Q × S, i.e. M = Q_q ∩ S_r.
;;
;; These two sets are equal: t ∈ qt implies t ∈ Q_q, so for any t in qt ∩ rt,
;; t ∈ S_r ⇔ t ∈ Q_q ∩ S_r. range-subtract-many returns a canonical sorted
;; disjoint decomposition, so equal time-sets give equal piece counts -
;; the bags line up too, not just the snapshot sets.
;;
;; Two properties of the operator pair make this work, and breaking either
;; should reintroduce counterexamples:
;;   (a) /drop-old replaces both inputs' valid-at columns with the single
;;       intersection qt ∩ st, leaving no leftover source-time columns that
;;       would mismatch in the outer range-except's key.
;;   (b) range-except is set-style in time: every matching right-side
;;       range is unioned into a single minus-set per key. That's what
;;       makes M = Q_q ∩ S_r.

(define (drop-lhs Q R S)
  (range-cartesian-product/drop-old
   'valid-at Q (range-except 'valid-at R S)))

(define (drop-rhs Q R S)
  (range-except
   'valid-at
   (range-cartesian-product/drop-old 'valid-at Q R)
   (range-cartesian-product/drop-old 'valid-at Q S)))

(define drop-cases
  (list
   ;; the canonical one
   (list "canonical"
         (rel Qd (list (tuple 'q1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   ;; Q has a "gap" in valid time
   (list "Q has gap"
         (rel Qd (list (tuple 'q1 '(0 . 5)) (tuple 'q1 '(10 . 15))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   ;; multiple Q rows with same q-id but different times
   (list "Q has 2 rows same id"
         (rel Qd (list (tuple 'q1 '(0 . 20)) (tuple 'q1 '(10 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(5 . 15)))))
   ;; R has duplicate rows
   (list "R has duplicates"
         (rel Qd (list (tuple 'q1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30)) (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(10 . 20)))))
   ;; S has duplicates (would double-subtract in a bag)
   (list "S has duplicates"
         (rel Qd (list (tuple 'q1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(10 . 20)) (tuple 'r1 '(10 . 20)))))
   ;; multiple S rows that fully cover R
   (list "S fully covers R"
         (rel Qd (list (tuple 'q1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 15)) (tuple 'r1 '(10 . 30)))))
   ;; R has overlapping pieces for the same r-id
   (list "R overlapping pieces same id"
         (rel Qd (list (tuple 'q1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 20)) (tuple 'r1 '(10 . 30))))
         (rel Rd (list (tuple 'r1 '(15 . 25)))))
   ;; Q trims S: Q is narrower than S
   (list "Q narrower than S"
         (rel Qd (list (tuple 'q1 '(0 . 10))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 15)))))
   ;; Q overlap with S "fits the gap"
   (list "Q two rows with gap; S fits gap"
         (rel Qd (list (tuple 'q1 '(0 . 5)) (tuple 'q1 '(10 . 15))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   ;; multiple Q rows + multiple S rows
   (list "Q×S with multiple rows on each side"
         (rel Qd (list (tuple 'q1 '(0 . 30)) (tuple 'q2 '(10 . 40))))
         (rel Rd (list (tuple 'r1 '(0 . 40)) (tuple 'r2 '(5 . 35))))
         (rel Rd (list (tuple 'r1 '(8 . 12)) (tuple 'r2 '(20 . 25)))))
   ;; empties
   (list "empty Q"
         (rel Qd '())
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   (list "empty R"
         (rel Qd (list (tuple 'q1 '(0 . 20))))
         (rel Rd '())
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   (list "empty S"
         (rel Qd (list (tuple 'q1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd '()))
   ;; touching ranges (no overlap by SQL OVERLAPS)
   (list "touching ranges"
         (rel Qd (list (tuple 'q1 '(0 . 10)) (tuple 'q1 '(10 . 20))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 15)))))
   ;; Q has time range that doesn't reach R at all
   (list "Q time disjoint from R"
         (rel Qd (list (tuple 'q1 '(50 . 60))))
         (rel Rd (list (tuple 'r1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 10)))))
   ;; R/S with an extra non-time column
   (list "extra non-time column"
         (rel Qd (list (tuple 'q1 '(0 . 20))))
         (rel Rd2 (list (tuple 'r1 "red" '(0 . 20)) (tuple 'r2 "blue" '(0 . 20))))
         (rel Rd2 (list (tuple 'r1 "red" '(5 . 15)))))
   ;; Q with a non-time column
   (list "Q with non-time column"
         (rel Qd2 (list (tuple 'q1 'A '(0 . 20)) (tuple 'q1 'B '(10 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(5 . 25)))))
   ;; bag bombing: Q,R,S all dup the same row
   (list "Q,R,S all duplicate"
         (rel Qd (list (tuple 'q1 '(0 . 30)) (tuple 'q1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(0 . 30)) (tuple 'r1 '(0 . 30))))
         (rel Rd (list (tuple 'r1 '(10 . 20)) (tuple 'r1 '(10 . 20)))))))

(define (rand-range-rel desc id-syms n)
  (rel desc (for/list ([_ (in-range n)])
              (tuple (rand-elt id-syms) (rand-range)))))

(define (drop-gen)
  (list (rand-range-rel Qd '(q1 q2 q3) (random 0 6))
        (rand-range-rel Rd '(r1 r2 r3) (random 0 6))
        (rand-range-rel Rd '(r1 r2 r3) (random 0 6))))

(probe "range-cartesian-product/drop-old distributes over range-except"
       drop-lhs drop-rhs drop-cases
       #:input-names '("Q" "R" "S")
       #:gen drop-gen #:trials 5000)

;; ===========================================================================
;; Experiment 2: does range-select distribute over range-cartesian-product?
;;   σ_p(Q × R) = (σ_p Q) × R,  where p only inspects Q's columns
;; ===========================================================================
;;
;; Selection on a left-only predicate should slide past the product: a result
;; row exists iff Q and R overlap and Q passes p, regardless of whether we
;; filter before or after pairing. q-id is column 0 in both Q and Q × R.

(define (q1? t) (eq? (list-ref (tuple-values t) 0) 'q1))

(define (sel-lhs Q R)
  (range-select q1? (range-cartesian-product 'valid-at Q R)))

(define (sel-rhs Q R)
  (range-cartesian-product 'valid-at (range-select q1? Q) R))

(define sel-cases
  (list
   (list "mixed q-ids, one R row"
         (rel Qd (list (tuple 'q1 '(0 . 20)) (tuple 'q2 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 15)))))
   (list "predicate keeps nothing"
         (rel Qd (list (tuple 'q2 '(0 . 20)) (tuple 'q3 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 15)))))
   (list "duplicate q1 rows"
         (rel Qd (list (tuple 'q1 '(0 . 20)) (tuple 'q1 '(0 . 20))))
         (rel Rd (list (tuple 'r1 '(5 . 15)) (tuple 'r2 '(0 . 30)))))
   (list "empty R"
         (rel Qd (list (tuple 'q1 '(0 . 20))))
         (rel Rd '()))))

(define (sel-gen)
  (list (rand-range-rel Qd '(q1 q2 q3) (random 0 6))
        (rand-range-rel Rd '(r1 r2 r3) (random 0 6))))

(probe "range-select distributes over range-cartesian-product"
       sel-lhs sel-rhs sel-cases
       #:input-names '("Q" "R")
       #:gen sel-gen #:trials 5000)
