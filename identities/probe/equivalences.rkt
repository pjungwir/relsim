#lang racket

;; Probe the classical relational-algebra equivalence rules (the ones in
;; identities/EquivalenceRules.pdf) against the temporal operators, to see
;; which survive temporalization. Each `probe` fuzzes an LHS vs RHS; a line
;; reading "fuzz: 0/N failures" means the identity held on every trial.
;;
;; Run with: racket identities/probe/equivalences.rkt

(require "harness.rkt"
         "../../relsim.rkt")

;; ---------------------------------------------------------------------------
;; Descs and random rel builders
;; ---------------------------------------------------------------------------

(define Qd (tuple-desc '(q-id valid-at)))
(define Rd (tuple-desc '(r-id valid-at)))
(define Sd (tuple-desc '(s-id valid-at)))
;; with a shared join key `k`
(define Qk (tuple-desc '(q-id k valid-at)))
(define Rk (tuple-desc '(r-id k valid-at)))

(define (rrel desc id-syms n)             ; range-valued rel
  (rel desc (for/list ([_ (in-range n)])
              (tuple (rand-elt id-syms) (rand-range)))))
(define (rrel-k desc id-syms n)           ; range rel with a join key column
  (rel desc (for/list ([_ (in-range n)])
              (tuple (rand-elt id-syms) (rand-elt '(k1 k2)) (rand-range)))))
(define (mrel desc id-syms n)             ; multirange-valued rel
  (rel desc (for/list ([_ (in-range n)])
              (tuple (rand-elt id-syms) (rand-multirange)))))
(define (trel desc id-syms n)             ; tquel rel (one tsattr column)
  (rel desc (for/list ([_ (in-range n)])
              (tuple (tsattr (rand-elt id-syms) (rand-multirange))))))

(define TRIALS 3000)

;; predicates used below (column 0 is the id in every desc here)
(define (id0=? sym) (lambda (t) (eq? (list-ref (tuple-values t) 0) sym)))
(define (tid0=? sym) (lambda (t) (eq? (tsattr-val (list-ref (tuple-values t) 0)) sym)))

;; ===========================================================================
;; Rule 5: commutativity of the product/join.   Q × R = R × Q
;; ===========================================================================
;; Our products append/keep columns positionally, so swapping the inputs
;; reorders the columns: equal only "up to column reordering", never on the
;; nose. (This is already true of plain cartesian-product in a positional
;; model; it is not a temporal effect.)

(probe "Rule 5 (range): range-cartesian-product is commutative"
       (lambda (Q R) (range-cartesian-product 'valid-at Q R))
       (lambda (Q R) (range-cartesian-product 'valid-at R Q))
       (list)
       #:gen (lambda () (list (rrel Qd '(q1 q2) (random 0 4))
                              (rrel Rd '(r1 r2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; ===========================================================================
;; Rule 6: associativity of the product.   (Q × R) × S = Q × (R × S)
;; ===========================================================================

;; range: the plain product appends a fresh valid-at column and a re-join
;; looks up the *leftmost* valid-at, so re-association uses a different
;; valid-time (and a different column order). Expected to FAIL.
(probe "Rule 6 (range): range-cartesian-product is associative"
       (lambda (Q R S) (range-cartesian-product 'valid-at
                          (range-cartesian-product 'valid-at Q R) S))
       (lambda (Q R S) (range-cartesian-product 'valid-at Q
                          (range-cartesian-product 'valid-at R S)))
       (list)
       #:gen (lambda () (list (rrel Qd '(q1 q2) (random 0 4))
                              (rrel Rd '(r1 r2) (random 0 4))
                              (rrel Sd '(s1 s2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; range /drop-old: collapses each pair to the single intersection, leaving
;; no stray valid-at column. Expected to HOLD (the same fix that restores
;; distributivity over difference).
(probe "Rule 6 (range, /drop-old): associative"
       (lambda (Q R S) (range-cartesian-product/drop-old 'valid-at
                          (range-cartesian-product/drop-old 'valid-at Q R) S))
       (lambda (Q R S) (range-cartesian-product/drop-old 'valid-at Q
                          (range-cartesian-product/drop-old 'valid-at R S)))
       (list)
       #:gen (lambda () (list (rrel Qd '(q1 q2) (random 0 4))
                              (rrel Rd '(r1 r2) (random 0 4))
                              (rrel Sd '(s1 s2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; multirange: same story as range.
(probe "Rule 6 (multirange): multirange-cartesian-product is associative"
       (lambda (Q R S) (multirange-cartesian-product 'valid-at
                          (multirange-cartesian-product 'valid-at Q R) S))
       (lambda (Q R S) (multirange-cartesian-product 'valid-at Q
                          (multirange-cartesian-product 'valid-at R S)))
       (list)
       #:gen (lambda () (list (mrel Qd '(q1 q2) (random 0 4))
                              (mrel Rd '(r1 r2) (random 0 4))
                              (mrel Sd '(s1 s2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

(probe "Rule 6 (multirange, /drop-old): associative"
       (lambda (Q R S) (multirange-cartesian-product/drop-old 'valid-at
                          (multirange-cartesian-product/drop-old 'valid-at Q R) S))
       (lambda (Q R S) (multirange-cartesian-product/drop-old 'valid-at Q
                          (multirange-cartesian-product/drop-old 'valid-at R S)))
       (list)
       #:gen (lambda () (list (mrel Qd '(q1 q2) (random 0 4))
                              (mrel Rd '(r1 r2) (random 0 4))
                              (mrel Sd '(s1 s2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; tquel: the product is plain cartesian-product (the time lives in the
;; attributes, not a separate column), so it is associative like classical
;; relational algebra. Expected to HOLD.
(probe "Rule 6 (tquel): tquel-cartesian-product is associative"
       (lambda (Q R S) (tquel-cartesian-product (tquel-cartesian-product Q R) S))
       (lambda (Q R S) (tquel-cartesian-product Q (tquel-cartesian-product R S)))
       (list)
       #:gen (lambda () (list (trel Qd '(q1 q2) (random 0 4))
                              (trel Rd '(r1 r2) (random 0 4))
                              (trel Sd '(s1 s2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; ===========================================================================
;; Rule 7(a): selection (left-only predicate) distributes over the join.
;;   σ_θ0(Q ⋈ R) = (σ_θ0 Q) ⋈ R
;; ===========================================================================
(define (kjoin t1 t2) (equal? (tuple-ref t1 Qk 'k) (tuple-ref t2 Rk 'k)))

(probe "Rule 7a (range): range-select distributes over range-join"
       (lambda (Q R) (range-select (id0=? 'q1) (range-join kjoin 'valid-at Q R)))
       (lambda (Q R) (range-join kjoin 'valid-at (range-select (id0=? 'q1) Q) R))
       (list)
       #:gen (lambda () (list (rrel-k Qk '(q1 q2) (random 0 4))
                              (rrel-k Rk '(r1 r2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

;; ===========================================================================
;; Rule 11: selection distributes over set difference.
;;   σ_P(R − S) = (σ_P R) − (σ_P S)
;; ===========================================================================
(probe "Rule 11 (range): range-select distributes over range-except"
       (lambda (R S) (range-select (id0=? 'r1) (range-except 'valid-at R S)))
       (lambda (R S) (range-except 'valid-at (range-select (id0=? 'r1) R)
                                              (range-select (id0=? 'r1) S)))
       (list)
       #:gen (lambda () (list (rrel Rd '(r1 r2) (random 0 4))
                              (rrel Rd '(r1 r2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

(probe "Rule 11 (multirange): multirange-select distributes over multirange-except"
       (lambda (R S) (multirange-select (id0=? 'r1) (multirange-except 'valid-at R S)))
       (lambda (R S) (multirange-except 'valid-at (multirange-select (id0=? 'r1) R)
                                                  (multirange-select (id0=? 'r1) S)))
       (list)
       #:gen (lambda () (list (mrel Rd '(r1 r2) (random 0 4))
                              (mrel Rd '(r1 r2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)

(probe "Rule 11 (tquel): tquel-select distributes over tquel-except"
       (lambda (R S) (tquel-select (tid0=? 'r1) (tquel-except R S)))
       (lambda (R S) (tquel-except (tquel-select (tid0=? 'r1) R)
                                   (tquel-select (tid0=? 'r1) S)))
       (list)
       #:gen (lambda () (list (trel Rd '(r1 r2) (random 0 4))
                              (trel Rd '(r1 r2) (random 0 4))))
       #:trials TRIALS #:max-dumps 1)
